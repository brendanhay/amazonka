{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Signer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Signer where

import Network.AWS.CloudFront.Types.KeyPairIds
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of AWS accounts and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
--
--
-- /See:/ 'signer' smart constructor.
data Signer = Signer'
  { _sAWSAccountNumber :: !(Maybe Text),
    _sKeyPairIds :: !(Maybe KeyPairIds)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Signer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAWSAccountNumber' - An AWS account number that contains active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If the AWS account that owns the key pairs is the same account that owns the CloudFront distribution, the value of this field is @self@ .
--
-- * 'sKeyPairIds' - A list of CloudFront key pair identifiers.
signer ::
  Signer
signer =
  Signer' {_sAWSAccountNumber = Nothing, _sKeyPairIds = Nothing}

-- | An AWS account number that contains active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If the AWS account that owns the key pairs is the same account that owns the CloudFront distribution, the value of this field is @self@ .
sAWSAccountNumber :: Lens' Signer (Maybe Text)
sAWSAccountNumber = lens _sAWSAccountNumber (\s a -> s {_sAWSAccountNumber = a})

-- | A list of CloudFront key pair identifiers.
sKeyPairIds :: Lens' Signer (Maybe KeyPairIds)
sKeyPairIds = lens _sKeyPairIds (\s a -> s {_sKeyPairIds = a})

instance FromXML Signer where
  parseXML x =
    Signer' <$> (x .@? "AwsAccountNumber") <*> (x .@? "KeyPairIds")

instance Hashable Signer

instance NFData Signer
