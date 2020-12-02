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
-- Module      : Network.AWS.Lightsail.DeleteDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail content delivery network (CDN) distribution.
module Network.AWS.Lightsail.DeleteDistribution
  ( -- * Creating a Request
    deleteDistribution,
    DeleteDistribution,

    -- * Request Lenses
    ddDistributionName,

    -- * Destructuring the Response
    deleteDistributionResponse,
    DeleteDistributionResponse,

    -- * Response Lenses
    dddrsOperation,
    dddrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDistribution' smart constructor.
newtype DeleteDistribution = DeleteDistribution'
  { _ddDistributionName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDistributionName' - The name of the distribution to delete. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
deleteDistribution ::
  DeleteDistribution
deleteDistribution =
  DeleteDistribution' {_ddDistributionName = Nothing}

-- | The name of the distribution to delete. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
ddDistributionName :: Lens' DeleteDistribution (Maybe Text)
ddDistributionName = lens _ddDistributionName (\s a -> s {_ddDistributionName = a})

instance AWSRequest DeleteDistribution where
  type Rs DeleteDistribution = DeleteDistributionResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          DeleteDistributionResponse'
            <$> (x .?> "operation") <*> (pure (fromEnum s))
      )

instance Hashable DeleteDistribution

instance NFData DeleteDistribution

instance ToHeaders DeleteDistribution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.DeleteDistribution" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteDistribution where
  toJSON DeleteDistribution' {..} =
    object
      (catMaybes [("distributionName" .=) <$> _ddDistributionName])

instance ToPath DeleteDistribution where
  toPath = const "/"

instance ToQuery DeleteDistribution where
  toQuery = const mempty

-- | /See:/ 'deleteDistributionResponse' smart constructor.
data DeleteDistributionResponse = DeleteDistributionResponse'
  { _dddrsOperation ::
      !(Maybe Operation),
    _dddrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dddrsOperation' - An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'dddrsResponseStatus' - -- | The response status code.
deleteDistributionResponse ::
  -- | 'dddrsResponseStatus'
  Int ->
  DeleteDistributionResponse
deleteDistributionResponse pResponseStatus_ =
  DeleteDistributionResponse'
    { _dddrsOperation = Nothing,
      _dddrsResponseStatus = pResponseStatus_
    }

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
dddrsOperation :: Lens' DeleteDistributionResponse (Maybe Operation)
dddrsOperation = lens _dddrsOperation (\s a -> s {_dddrsOperation = a})

-- | -- | The response status code.
dddrsResponseStatus :: Lens' DeleteDistributionResponse Int
dddrsResponseStatus = lens _dddrsResponseStatus (\s a -> s {_dddrsResponseStatus = a})

instance NFData DeleteDistributionResponse
