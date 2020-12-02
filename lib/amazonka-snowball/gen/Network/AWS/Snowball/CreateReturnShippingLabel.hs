{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CreateReturnShippingLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a shipping label that will be used to return the Snow device to AWS.
module Network.AWS.Snowball.CreateReturnShippingLabel
  ( -- * Creating a Request
    createReturnShippingLabel,
    CreateReturnShippingLabel,

    -- * Request Lenses
    crslShippingOption,
    crslJobId,

    -- * Destructuring the Response
    createReturnShippingLabelResponse,
    CreateReturnShippingLabelResponse,

    -- * Response Lenses
    crslrsStatus,
    crslrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types

-- | /See:/ 'createReturnShippingLabel' smart constructor.
data CreateReturnShippingLabel = CreateReturnShippingLabel'
  { _crslShippingOption ::
      !(Maybe ShippingOption),
    _crslJobId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateReturnShippingLabel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crslShippingOption' - The shipping speed for a particular job. This speed doesn't dictate how soon the device is returned to AWS. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:
--
-- * 'crslJobId' - The ID for a job that you want to create the return shipping label for. For example @JID123e4567-e89b-12d3-a456-426655440000@ .
createReturnShippingLabel ::
  -- | 'crslJobId'
  Text ->
  CreateReturnShippingLabel
createReturnShippingLabel pJobId_ =
  CreateReturnShippingLabel'
    { _crslShippingOption = Nothing,
      _crslJobId = pJobId_
    }

-- | The shipping speed for a particular job. This speed doesn't dictate how soon the device is returned to AWS. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:
crslShippingOption :: Lens' CreateReturnShippingLabel (Maybe ShippingOption)
crslShippingOption = lens _crslShippingOption (\s a -> s {_crslShippingOption = a})

-- | The ID for a job that you want to create the return shipping label for. For example @JID123e4567-e89b-12d3-a456-426655440000@ .
crslJobId :: Lens' CreateReturnShippingLabel Text
crslJobId = lens _crslJobId (\s a -> s {_crslJobId = a})

instance AWSRequest CreateReturnShippingLabel where
  type
    Rs CreateReturnShippingLabel =
      CreateReturnShippingLabelResponse
  request = postJSON snowball
  response =
    receiveJSON
      ( \s h x ->
          CreateReturnShippingLabelResponse'
            <$> (x .?> "Status") <*> (pure (fromEnum s))
      )

instance Hashable CreateReturnShippingLabel

instance NFData CreateReturnShippingLabel

instance ToHeaders CreateReturnShippingLabel where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSIESnowballJobManagementService.CreateReturnShippingLabel" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateReturnShippingLabel where
  toJSON CreateReturnShippingLabel' {..} =
    object
      ( catMaybes
          [ ("ShippingOption" .=) <$> _crslShippingOption,
            Just ("JobId" .= _crslJobId)
          ]
      )

instance ToPath CreateReturnShippingLabel where
  toPath = const "/"

instance ToQuery CreateReturnShippingLabel where
  toQuery = const mempty

-- | /See:/ 'createReturnShippingLabelResponse' smart constructor.
data CreateReturnShippingLabelResponse = CreateReturnShippingLabelResponse'
  { _crslrsStatus ::
      !( Maybe
           ShippingLabelStatus
       ),
    _crslrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateReturnShippingLabelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crslrsStatus' - The status information of the task on a Snow device that is being returned to AWS.
--
-- * 'crslrsResponseStatus' - -- | The response status code.
createReturnShippingLabelResponse ::
  -- | 'crslrsResponseStatus'
  Int ->
  CreateReturnShippingLabelResponse
createReturnShippingLabelResponse pResponseStatus_ =
  CreateReturnShippingLabelResponse'
    { _crslrsStatus = Nothing,
      _crslrsResponseStatus = pResponseStatus_
    }

-- | The status information of the task on a Snow device that is being returned to AWS.
crslrsStatus :: Lens' CreateReturnShippingLabelResponse (Maybe ShippingLabelStatus)
crslrsStatus = lens _crslrsStatus (\s a -> s {_crslrsStatus = a})

-- | -- | The response status code.
crslrsResponseStatus :: Lens' CreateReturnShippingLabelResponse Int
crslrsResponseStatus = lens _crslrsResponseStatus (\s a -> s {_crslrsResponseStatus = a})

instance NFData CreateReturnShippingLabelResponse
