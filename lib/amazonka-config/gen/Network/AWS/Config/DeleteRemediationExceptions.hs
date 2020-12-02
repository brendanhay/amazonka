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
-- Module      : Network.AWS.Config.DeleteRemediationExceptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more remediation exceptions mentioned in the resource keys.
module Network.AWS.Config.DeleteRemediationExceptions
  ( -- * Creating a Request
    deleteRemediationExceptions,
    DeleteRemediationExceptions,

    -- * Request Lenses
    dConfigRuleName,
    dResourceKeys,

    -- * Destructuring the Response
    deleteRemediationExceptionsResponse,
    DeleteRemediationExceptionsResponse,

    -- * Response Lenses
    delrsFailedBatches,
    delrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRemediationExceptions' smart constructor.
data DeleteRemediationExceptions = DeleteRemediationExceptions'
  { _dConfigRuleName ::
      !Text,
    _dResourceKeys ::
      !( List1
           RemediationExceptionResourceKey
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRemediationExceptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dConfigRuleName' - The name of the AWS Config rule for which you want to delete remediation exception configuration.
--
-- * 'dResourceKeys' - An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
deleteRemediationExceptions ::
  -- | 'dConfigRuleName'
  Text ->
  -- | 'dResourceKeys'
  NonEmpty RemediationExceptionResourceKey ->
  DeleteRemediationExceptions
deleteRemediationExceptions pConfigRuleName_ pResourceKeys_ =
  DeleteRemediationExceptions'
    { _dConfigRuleName = pConfigRuleName_,
      _dResourceKeys = _List1 # pResourceKeys_
    }

-- | The name of the AWS Config rule for which you want to delete remediation exception configuration.
dConfigRuleName :: Lens' DeleteRemediationExceptions Text
dConfigRuleName = lens _dConfigRuleName (\s a -> s {_dConfigRuleName = a})

-- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
dResourceKeys :: Lens' DeleteRemediationExceptions (NonEmpty RemediationExceptionResourceKey)
dResourceKeys = lens _dResourceKeys (\s a -> s {_dResourceKeys = a}) . _List1

instance AWSRequest DeleteRemediationExceptions where
  type
    Rs DeleteRemediationExceptions =
      DeleteRemediationExceptionsResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          DeleteRemediationExceptionsResponse'
            <$> (x .?> "FailedBatches" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DeleteRemediationExceptions

instance NFData DeleteRemediationExceptions

instance ToHeaders DeleteRemediationExceptions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StarlingDoveService.DeleteRemediationExceptions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteRemediationExceptions where
  toJSON DeleteRemediationExceptions' {..} =
    object
      ( catMaybes
          [ Just ("ConfigRuleName" .= _dConfigRuleName),
            Just ("ResourceKeys" .= _dResourceKeys)
          ]
      )

instance ToPath DeleteRemediationExceptions where
  toPath = const "/"

instance ToQuery DeleteRemediationExceptions where
  toQuery = const mempty

-- | /See:/ 'deleteRemediationExceptionsResponse' smart constructor.
data DeleteRemediationExceptionsResponse = DeleteRemediationExceptionsResponse'
  { _delrsFailedBatches ::
      !( Maybe
           [FailedDeleteRemediationExceptionsBatch]
       ),
    _delrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRemediationExceptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsFailedBatches' - Returns a list of failed delete remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteRemediationExceptionsResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteRemediationExceptionsResponse
deleteRemediationExceptionsResponse pResponseStatus_ =
  DeleteRemediationExceptionsResponse'
    { _delrsFailedBatches =
        Nothing,
      _delrsResponseStatus = pResponseStatus_
    }

-- | Returns a list of failed delete remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
delrsFailedBatches :: Lens' DeleteRemediationExceptionsResponse [FailedDeleteRemediationExceptionsBatch]
delrsFailedBatches = lens _delrsFailedBatches (\s a -> s {_delrsFailedBatches = a}) . _Default . _Coerce

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteRemediationExceptionsResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteRemediationExceptionsResponse
