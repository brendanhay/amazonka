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
-- Module      : Network.AWS.IoT.CreateAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Device Defender audit suppression.
module Network.AWS.IoT.CreateAuditSuppression
  ( -- * Creating a Request
    createAuditSuppression,
    CreateAuditSuppression,

    -- * Request Lenses
    casExpirationDate,
    casSuppressIndefinitely,
    casDescription,
    casCheckName,
    casResourceIdentifier,
    casClientRequestToken,

    -- * Destructuring the Response
    createAuditSuppressionResponse,
    CreateAuditSuppressionResponse,

    -- * Response Lenses
    casrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAuditSuppression' smart constructor.
data CreateAuditSuppression = CreateAuditSuppression'
  { _casExpirationDate ::
      !(Maybe POSIX),
    _casSuppressIndefinitely :: !(Maybe Bool),
    _casDescription :: !(Maybe Text),
    _casCheckName :: !Text,
    _casResourceIdentifier :: !ResourceIdentifier,
    _casClientRequestToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAuditSuppression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'casExpirationDate' - The epoch timestamp in seconds at which this suppression expires.
--
-- * 'casSuppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- * 'casDescription' - The description of the audit suppression.
--
-- * 'casCheckName' - Undocumented member.
--
-- * 'casResourceIdentifier' - Undocumented member.
--
-- * 'casClientRequestToken' - The epoch timestamp in seconds at which this suppression expires.
createAuditSuppression ::
  -- | 'casCheckName'
  Text ->
  -- | 'casResourceIdentifier'
  ResourceIdentifier ->
  -- | 'casClientRequestToken'
  Text ->
  CreateAuditSuppression
createAuditSuppression
  pCheckName_
  pResourceIdentifier_
  pClientRequestToken_ =
    CreateAuditSuppression'
      { _casExpirationDate = Nothing,
        _casSuppressIndefinitely = Nothing,
        _casDescription = Nothing,
        _casCheckName = pCheckName_,
        _casResourceIdentifier = pResourceIdentifier_,
        _casClientRequestToken = pClientRequestToken_
      }

-- | The epoch timestamp in seconds at which this suppression expires.
casExpirationDate :: Lens' CreateAuditSuppression (Maybe UTCTime)
casExpirationDate = lens _casExpirationDate (\s a -> s {_casExpirationDate = a}) . mapping _Time

-- | Indicates whether a suppression should exist indefinitely or not.
casSuppressIndefinitely :: Lens' CreateAuditSuppression (Maybe Bool)
casSuppressIndefinitely = lens _casSuppressIndefinitely (\s a -> s {_casSuppressIndefinitely = a})

-- | The description of the audit suppression.
casDescription :: Lens' CreateAuditSuppression (Maybe Text)
casDescription = lens _casDescription (\s a -> s {_casDescription = a})

-- | Undocumented member.
casCheckName :: Lens' CreateAuditSuppression Text
casCheckName = lens _casCheckName (\s a -> s {_casCheckName = a})

-- | Undocumented member.
casResourceIdentifier :: Lens' CreateAuditSuppression ResourceIdentifier
casResourceIdentifier = lens _casResourceIdentifier (\s a -> s {_casResourceIdentifier = a})

-- | The epoch timestamp in seconds at which this suppression expires.
casClientRequestToken :: Lens' CreateAuditSuppression Text
casClientRequestToken = lens _casClientRequestToken (\s a -> s {_casClientRequestToken = a})

instance AWSRequest CreateAuditSuppression where
  type Rs CreateAuditSuppression = CreateAuditSuppressionResponse
  request = postJSON ioT
  response =
    receiveEmpty
      ( \s h x ->
          CreateAuditSuppressionResponse' <$> (pure (fromEnum s))
      )

instance Hashable CreateAuditSuppression

instance NFData CreateAuditSuppression

instance ToHeaders CreateAuditSuppression where
  toHeaders = const mempty

instance ToJSON CreateAuditSuppression where
  toJSON CreateAuditSuppression' {..} =
    object
      ( catMaybes
          [ ("expirationDate" .=) <$> _casExpirationDate,
            ("suppressIndefinitely" .=) <$> _casSuppressIndefinitely,
            ("description" .=) <$> _casDescription,
            Just ("checkName" .= _casCheckName),
            Just ("resourceIdentifier" .= _casResourceIdentifier),
            Just ("clientRequestToken" .= _casClientRequestToken)
          ]
      )

instance ToPath CreateAuditSuppression where
  toPath = const "/audit/suppressions/create"

instance ToQuery CreateAuditSuppression where
  toQuery = const mempty

-- | /See:/ 'createAuditSuppressionResponse' smart constructor.
newtype CreateAuditSuppressionResponse = CreateAuditSuppressionResponse'
  { _casrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAuditSuppressionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'casrsResponseStatus' - -- | The response status code.
createAuditSuppressionResponse ::
  -- | 'casrsResponseStatus'
  Int ->
  CreateAuditSuppressionResponse
createAuditSuppressionResponse pResponseStatus_ =
  CreateAuditSuppressionResponse'
    { _casrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
casrsResponseStatus :: Lens' CreateAuditSuppressionResponse Int
casrsResponseStatus = lens _casrsResponseStatus (\s a -> s {_casrsResponseStatus = a})

instance NFData CreateAuditSuppressionResponse
