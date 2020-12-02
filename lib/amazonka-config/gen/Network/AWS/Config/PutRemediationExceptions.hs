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
-- Module      : Network.AWS.Config.PutRemediationExceptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A remediation exception is when a specific resource is no longer considered for auto-remediation. This API adds a new exception or updates an exisiting exception for a specific resource with a specific AWS Config rule.
module Network.AWS.Config.PutRemediationExceptions
  ( -- * Creating a Request
    putRemediationExceptions,
    PutRemediationExceptions,

    -- * Request Lenses
    preMessage,
    preExpirationTime,
    preConfigRuleName,
    preResourceKeys,

    -- * Destructuring the Response
    putRemediationExceptionsResponse,
    PutRemediationExceptionsResponse,

    -- * Response Lenses
    prersFailedBatches,
    prersResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putRemediationExceptions' smart constructor.
data PutRemediationExceptions = PutRemediationExceptions'
  { _preMessage ::
      !(Maybe Text),
    _preExpirationTime :: !(Maybe POSIX),
    _preConfigRuleName :: !Text,
    _preResourceKeys ::
      !(List1 RemediationExceptionResourceKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutRemediationExceptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'preMessage' - The message contains an explanation of the exception.
--
-- * 'preExpirationTime' - The exception is automatically deleted after the expiration date.
--
-- * 'preConfigRuleName' - The name of the AWS Config rule for which you want to create remediation exception.
--
-- * 'preResourceKeys' - An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
putRemediationExceptions ::
  -- | 'preConfigRuleName'
  Text ->
  -- | 'preResourceKeys'
  NonEmpty RemediationExceptionResourceKey ->
  PutRemediationExceptions
putRemediationExceptions pConfigRuleName_ pResourceKeys_ =
  PutRemediationExceptions'
    { _preMessage = Nothing,
      _preExpirationTime = Nothing,
      _preConfigRuleName = pConfigRuleName_,
      _preResourceKeys = _List1 # pResourceKeys_
    }

-- | The message contains an explanation of the exception.
preMessage :: Lens' PutRemediationExceptions (Maybe Text)
preMessage = lens _preMessage (\s a -> s {_preMessage = a})

-- | The exception is automatically deleted after the expiration date.
preExpirationTime :: Lens' PutRemediationExceptions (Maybe UTCTime)
preExpirationTime = lens _preExpirationTime (\s a -> s {_preExpirationTime = a}) . mapping _Time

-- | The name of the AWS Config rule for which you want to create remediation exception.
preConfigRuleName :: Lens' PutRemediationExceptions Text
preConfigRuleName = lens _preConfigRuleName (\s a -> s {_preConfigRuleName = a})

-- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
preResourceKeys :: Lens' PutRemediationExceptions (NonEmpty RemediationExceptionResourceKey)
preResourceKeys = lens _preResourceKeys (\s a -> s {_preResourceKeys = a}) . _List1

instance AWSRequest PutRemediationExceptions where
  type Rs PutRemediationExceptions = PutRemediationExceptionsResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          PutRemediationExceptionsResponse'
            <$> (x .?> "FailedBatches" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable PutRemediationExceptions

instance NFData PutRemediationExceptions

instance ToHeaders PutRemediationExceptions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StarlingDoveService.PutRemediationExceptions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutRemediationExceptions where
  toJSON PutRemediationExceptions' {..} =
    object
      ( catMaybes
          [ ("Message" .=) <$> _preMessage,
            ("ExpirationTime" .=) <$> _preExpirationTime,
            Just ("ConfigRuleName" .= _preConfigRuleName),
            Just ("ResourceKeys" .= _preResourceKeys)
          ]
      )

instance ToPath PutRemediationExceptions where
  toPath = const "/"

instance ToQuery PutRemediationExceptions where
  toQuery = const mempty

-- | /See:/ 'putRemediationExceptionsResponse' smart constructor.
data PutRemediationExceptionsResponse = PutRemediationExceptionsResponse'
  { _prersFailedBatches ::
      !( Maybe
           [FailedRemediationExceptionBatch]
       ),
    _prersResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutRemediationExceptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prersFailedBatches' - Returns a list of failed remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
--
-- * 'prersResponseStatus' - -- | The response status code.
putRemediationExceptionsResponse ::
  -- | 'prersResponseStatus'
  Int ->
  PutRemediationExceptionsResponse
putRemediationExceptionsResponse pResponseStatus_ =
  PutRemediationExceptionsResponse'
    { _prersFailedBatches = Nothing,
      _prersResponseStatus = pResponseStatus_
    }

-- | Returns a list of failed remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
prersFailedBatches :: Lens' PutRemediationExceptionsResponse [FailedRemediationExceptionBatch]
prersFailedBatches = lens _prersFailedBatches (\s a -> s {_prersFailedBatches = a}) . _Default . _Coerce

-- | -- | The response status code.
prersResponseStatus :: Lens' PutRemediationExceptionsResponse Int
prersResponseStatus = lens _prersResponseStatus (\s a -> s {_prersResponseStatus = a})

instance NFData PutRemediationExceptionsResponse
