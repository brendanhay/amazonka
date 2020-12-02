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
-- Module      : Network.AWS.IoT.DescribeAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender audit suppression.
module Network.AWS.IoT.DescribeAuditSuppression
  ( -- * Creating a Request
    describeAuditSuppression,
    DescribeAuditSuppression,

    -- * Request Lenses
    dCheckName,
    dResourceIdentifier,

    -- * Destructuring the Response
    describeAuditSuppressionResponse,
    DescribeAuditSuppressionResponse,

    -- * Response Lenses
    dasarsCheckName,
    dasarsExpirationDate,
    dasarsSuppressIndefinitely,
    dasarsDescription,
    dasarsResourceIdentifier,
    dasarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAuditSuppression' smart constructor.
data DescribeAuditSuppression = DescribeAuditSuppression'
  { _dCheckName ::
      !Text,
    _dResourceIdentifier ::
      !ResourceIdentifier
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAuditSuppression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCheckName' - Undocumented member.
--
-- * 'dResourceIdentifier' - Undocumented member.
describeAuditSuppression ::
  -- | 'dCheckName'
  Text ->
  -- | 'dResourceIdentifier'
  ResourceIdentifier ->
  DescribeAuditSuppression
describeAuditSuppression pCheckName_ pResourceIdentifier_ =
  DescribeAuditSuppression'
    { _dCheckName = pCheckName_,
      _dResourceIdentifier = pResourceIdentifier_
    }

-- | Undocumented member.
dCheckName :: Lens' DescribeAuditSuppression Text
dCheckName = lens _dCheckName (\s a -> s {_dCheckName = a})

-- | Undocumented member.
dResourceIdentifier :: Lens' DescribeAuditSuppression ResourceIdentifier
dResourceIdentifier = lens _dResourceIdentifier (\s a -> s {_dResourceIdentifier = a})

instance AWSRequest DescribeAuditSuppression where
  type Rs DescribeAuditSuppression = DescribeAuditSuppressionResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeAuditSuppressionResponse'
            <$> (x .?> "checkName")
            <*> (x .?> "expirationDate")
            <*> (x .?> "suppressIndefinitely")
            <*> (x .?> "description")
            <*> (x .?> "resourceIdentifier")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAuditSuppression

instance NFData DescribeAuditSuppression

instance ToHeaders DescribeAuditSuppression where
  toHeaders = const mempty

instance ToJSON DescribeAuditSuppression where
  toJSON DescribeAuditSuppression' {..} =
    object
      ( catMaybes
          [ Just ("checkName" .= _dCheckName),
            Just ("resourceIdentifier" .= _dResourceIdentifier)
          ]
      )

instance ToPath DescribeAuditSuppression where
  toPath = const "/audit/suppressions/describe"

instance ToQuery DescribeAuditSuppression where
  toQuery = const mempty

-- | /See:/ 'describeAuditSuppressionResponse' smart constructor.
data DescribeAuditSuppressionResponse = DescribeAuditSuppressionResponse'
  { _dasarsCheckName ::
      !(Maybe Text),
    _dasarsExpirationDate ::
      !(Maybe POSIX),
    _dasarsSuppressIndefinitely ::
      !(Maybe Bool),
    _dasarsDescription ::
      !(Maybe Text),
    _dasarsResourceIdentifier ::
      !( Maybe
           ResourceIdentifier
       ),
    _dasarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAuditSuppressionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasarsCheckName' - Undocumented member.
--
-- * 'dasarsExpirationDate' - The epoch timestamp in seconds at which this suppression expires.
--
-- * 'dasarsSuppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- * 'dasarsDescription' - The description of the audit suppression.
--
-- * 'dasarsResourceIdentifier' - Undocumented member.
--
-- * 'dasarsResponseStatus' - -- | The response status code.
describeAuditSuppressionResponse ::
  -- | 'dasarsResponseStatus'
  Int ->
  DescribeAuditSuppressionResponse
describeAuditSuppressionResponse pResponseStatus_ =
  DescribeAuditSuppressionResponse'
    { _dasarsCheckName = Nothing,
      _dasarsExpirationDate = Nothing,
      _dasarsSuppressIndefinitely = Nothing,
      _dasarsDescription = Nothing,
      _dasarsResourceIdentifier = Nothing,
      _dasarsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dasarsCheckName :: Lens' DescribeAuditSuppressionResponse (Maybe Text)
dasarsCheckName = lens _dasarsCheckName (\s a -> s {_dasarsCheckName = a})

-- | The epoch timestamp in seconds at which this suppression expires.
dasarsExpirationDate :: Lens' DescribeAuditSuppressionResponse (Maybe UTCTime)
dasarsExpirationDate = lens _dasarsExpirationDate (\s a -> s {_dasarsExpirationDate = a}) . mapping _Time

-- | Indicates whether a suppression should exist indefinitely or not.
dasarsSuppressIndefinitely :: Lens' DescribeAuditSuppressionResponse (Maybe Bool)
dasarsSuppressIndefinitely = lens _dasarsSuppressIndefinitely (\s a -> s {_dasarsSuppressIndefinitely = a})

-- | The description of the audit suppression.
dasarsDescription :: Lens' DescribeAuditSuppressionResponse (Maybe Text)
dasarsDescription = lens _dasarsDescription (\s a -> s {_dasarsDescription = a})

-- | Undocumented member.
dasarsResourceIdentifier :: Lens' DescribeAuditSuppressionResponse (Maybe ResourceIdentifier)
dasarsResourceIdentifier = lens _dasarsResourceIdentifier (\s a -> s {_dasarsResourceIdentifier = a})

-- | -- | The response status code.
dasarsResponseStatus :: Lens' DescribeAuditSuppressionResponse Int
dasarsResponseStatus = lens _dasarsResponseStatus (\s a -> s {_dasarsResponseStatus = a})

instance NFData DescribeAuditSuppressionResponse
