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
-- Module      : Network.AWS.Glue.PutWorkflowRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the specified workflow run properties for the given workflow run. If a property already exists for the specified run, then it overrides the value otherwise adds the property to existing properties.
module Network.AWS.Glue.PutWorkflowRunProperties
  ( -- * Creating a Request
    putWorkflowRunProperties,
    PutWorkflowRunProperties,

    -- * Request Lenses
    pwrpName,
    pwrpRunId,
    pwrpRunProperties,

    -- * Destructuring the Response
    putWorkflowRunPropertiesResponse,
    PutWorkflowRunPropertiesResponse,

    -- * Response Lenses
    pwrprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putWorkflowRunProperties' smart constructor.
data PutWorkflowRunProperties = PutWorkflowRunProperties'
  { _pwrpName ::
      !Text,
    _pwrpRunId :: !Text,
    _pwrpRunProperties :: !(Map Text (Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutWorkflowRunProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwrpName' - Name of the workflow which was run.
--
-- * 'pwrpRunId' - The ID of the workflow run for which the run properties should be updated.
--
-- * 'pwrpRunProperties' - The properties to put for the specified run.
putWorkflowRunProperties ::
  -- | 'pwrpName'
  Text ->
  -- | 'pwrpRunId'
  Text ->
  PutWorkflowRunProperties
putWorkflowRunProperties pName_ pRunId_ =
  PutWorkflowRunProperties'
    { _pwrpName = pName_,
      _pwrpRunId = pRunId_,
      _pwrpRunProperties = mempty
    }

-- | Name of the workflow which was run.
pwrpName :: Lens' PutWorkflowRunProperties Text
pwrpName = lens _pwrpName (\s a -> s {_pwrpName = a})

-- | The ID of the workflow run for which the run properties should be updated.
pwrpRunId :: Lens' PutWorkflowRunProperties Text
pwrpRunId = lens _pwrpRunId (\s a -> s {_pwrpRunId = a})

-- | The properties to put for the specified run.
pwrpRunProperties :: Lens' PutWorkflowRunProperties (HashMap Text (Text))
pwrpRunProperties = lens _pwrpRunProperties (\s a -> s {_pwrpRunProperties = a}) . _Map

instance AWSRequest PutWorkflowRunProperties where
  type Rs PutWorkflowRunProperties = PutWorkflowRunPropertiesResponse
  request = postJSON glue
  response =
    receiveEmpty
      ( \s h x ->
          PutWorkflowRunPropertiesResponse' <$> (pure (fromEnum s))
      )

instance Hashable PutWorkflowRunProperties

instance NFData PutWorkflowRunProperties

instance ToHeaders PutWorkflowRunProperties where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.PutWorkflowRunProperties" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutWorkflowRunProperties where
  toJSON PutWorkflowRunProperties' {..} =
    object
      ( catMaybes
          [ Just ("Name" .= _pwrpName),
            Just ("RunId" .= _pwrpRunId),
            Just ("RunProperties" .= _pwrpRunProperties)
          ]
      )

instance ToPath PutWorkflowRunProperties where
  toPath = const "/"

instance ToQuery PutWorkflowRunProperties where
  toQuery = const mempty

-- | /See:/ 'putWorkflowRunPropertiesResponse' smart constructor.
newtype PutWorkflowRunPropertiesResponse = PutWorkflowRunPropertiesResponse'
  { _pwrprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutWorkflowRunPropertiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwrprsResponseStatus' - -- | The response status code.
putWorkflowRunPropertiesResponse ::
  -- | 'pwrprsResponseStatus'
  Int ->
  PutWorkflowRunPropertiesResponse
putWorkflowRunPropertiesResponse pResponseStatus_ =
  PutWorkflowRunPropertiesResponse'
    { _pwrprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
pwrprsResponseStatus :: Lens' PutWorkflowRunPropertiesResponse Int
pwrprsResponseStatus = lens _pwrprsResponseStatus (\s a -> s {_pwrprsResponseStatus = a})

instance NFData PutWorkflowRunPropertiesResponse
