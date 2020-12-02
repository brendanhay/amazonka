{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.BlockPublicAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.BlockPublicAccess where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on how the bucker owner's S3 Block Public Access settings are being applied to the S3 bucket. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html S3 Block Public Access> for more information.
--
--
--
-- /See:/ 'blockPublicAccess' smart constructor.
data BlockPublicAccess = BlockPublicAccess'
  { _bpaIgnorePublicACLs ::
      !(Maybe Bool),
    _bpaBlockPublicACLs :: !(Maybe Bool),
    _bpaRestrictPublicBuckets :: !(Maybe Bool),
    _bpaBlockPublicPolicy :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BlockPublicAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpaIgnorePublicACLs' - Indicates if S3 Block Public Access is set to @IgnorePublicAcls@ .
--
-- * 'bpaBlockPublicACLs' - Indicates if S3 Block Public Access is set to @BlockPublicAcls@ .
--
-- * 'bpaRestrictPublicBuckets' - Indicates if S3 Block Public Access is set to @RestrictPublicBuckets@ .
--
-- * 'bpaBlockPublicPolicy' - Indicates if S3 Block Public Access is set to @BlockPublicPolicy@ .
blockPublicAccess ::
  BlockPublicAccess
blockPublicAccess =
  BlockPublicAccess'
    { _bpaIgnorePublicACLs = Nothing,
      _bpaBlockPublicACLs = Nothing,
      _bpaRestrictPublicBuckets = Nothing,
      _bpaBlockPublicPolicy = Nothing
    }

-- | Indicates if S3 Block Public Access is set to @IgnorePublicAcls@ .
bpaIgnorePublicACLs :: Lens' BlockPublicAccess (Maybe Bool)
bpaIgnorePublicACLs = lens _bpaIgnorePublicACLs (\s a -> s {_bpaIgnorePublicACLs = a})

-- | Indicates if S3 Block Public Access is set to @BlockPublicAcls@ .
bpaBlockPublicACLs :: Lens' BlockPublicAccess (Maybe Bool)
bpaBlockPublicACLs = lens _bpaBlockPublicACLs (\s a -> s {_bpaBlockPublicACLs = a})

-- | Indicates if S3 Block Public Access is set to @RestrictPublicBuckets@ .
bpaRestrictPublicBuckets :: Lens' BlockPublicAccess (Maybe Bool)
bpaRestrictPublicBuckets = lens _bpaRestrictPublicBuckets (\s a -> s {_bpaRestrictPublicBuckets = a})

-- | Indicates if S3 Block Public Access is set to @BlockPublicPolicy@ .
bpaBlockPublicPolicy :: Lens' BlockPublicAccess (Maybe Bool)
bpaBlockPublicPolicy = lens _bpaBlockPublicPolicy (\s a -> s {_bpaBlockPublicPolicy = a})

instance FromJSON BlockPublicAccess where
  parseJSON =
    withObject
      "BlockPublicAccess"
      ( \x ->
          BlockPublicAccess'
            <$> (x .:? "ignorePublicAcls")
            <*> (x .:? "blockPublicAcls")
            <*> (x .:? "restrictPublicBuckets")
            <*> (x .:? "blockPublicPolicy")
      )

instance Hashable BlockPublicAccess

instance NFData BlockPublicAccess
