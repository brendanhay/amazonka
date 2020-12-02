{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.PublicAccessBlockConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.PublicAccessBlockConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | The PublicAccessBlock configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'publicAccessBlockConfiguration' smart constructor.
data PublicAccessBlockConfiguration = PublicAccessBlockConfiguration'
  { _pabcIgnorePublicACLs ::
      !(Maybe Bool),
    _pabcBlockPublicACLs ::
      !(Maybe Bool),
    _pabcRestrictPublicBuckets ::
      !(Maybe Bool),
    _pabcBlockPublicPolicy ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicAccessBlockConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pabcIgnorePublicACLs' - Specifies whether Amazon S3 should ignore public ACLs for this bucket and objects in this bucket. Setting this element to @TRUE@ causes Amazon S3 to ignore all public ACLs on this bucket and objects in this bucket. Enabling this setting doesn't affect the persistence of any existing ACLs and doesn't prevent new public ACLs from being set.
--
-- * 'pabcBlockPublicACLs' - Specifies whether Amazon S3 should block public access control lists (ACLs) for this bucket and objects in this bucket. Setting this element to @TRUE@ causes the following behavior:     * PUT Bucket acl and PUT Object acl calls fail if the specified ACL is public.     * PUT Object calls fail if the request includes a public ACL.     * PUT Bucket calls fail if the request includes a public ACL. Enabling this setting doesn't affect existing policies or ACLs.
--
-- * 'pabcRestrictPublicBuckets' - Specifies whether Amazon S3 should restrict public bucket policies for this bucket. Setting this element to @TRUE@ restricts access to this bucket to only AWS service principals and authorized users within this account if the bucket has a public policy. Enabling this setting doesn't affect previously stored bucket policies, except that public and cross-account access within any public bucket policy, including non-public delegation to specific accounts, is blocked.
--
-- * 'pabcBlockPublicPolicy' - Specifies whether Amazon S3 should block public bucket policies for this bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls to PUT Bucket policy if the specified bucket policy allows public access.  Enabling this setting doesn't affect existing bucket policies.
publicAccessBlockConfiguration ::
  PublicAccessBlockConfiguration
publicAccessBlockConfiguration =
  PublicAccessBlockConfiguration'
    { _pabcIgnorePublicACLs = Nothing,
      _pabcBlockPublicACLs = Nothing,
      _pabcRestrictPublicBuckets = Nothing,
      _pabcBlockPublicPolicy = Nothing
    }

-- | Specifies whether Amazon S3 should ignore public ACLs for this bucket and objects in this bucket. Setting this element to @TRUE@ causes Amazon S3 to ignore all public ACLs on this bucket and objects in this bucket. Enabling this setting doesn't affect the persistence of any existing ACLs and doesn't prevent new public ACLs from being set.
pabcIgnorePublicACLs :: Lens' PublicAccessBlockConfiguration (Maybe Bool)
pabcIgnorePublicACLs = lens _pabcIgnorePublicACLs (\s a -> s {_pabcIgnorePublicACLs = a})

-- | Specifies whether Amazon S3 should block public access control lists (ACLs) for this bucket and objects in this bucket. Setting this element to @TRUE@ causes the following behavior:     * PUT Bucket acl and PUT Object acl calls fail if the specified ACL is public.     * PUT Object calls fail if the request includes a public ACL.     * PUT Bucket calls fail if the request includes a public ACL. Enabling this setting doesn't affect existing policies or ACLs.
pabcBlockPublicACLs :: Lens' PublicAccessBlockConfiguration (Maybe Bool)
pabcBlockPublicACLs = lens _pabcBlockPublicACLs (\s a -> s {_pabcBlockPublicACLs = a})

-- | Specifies whether Amazon S3 should restrict public bucket policies for this bucket. Setting this element to @TRUE@ restricts access to this bucket to only AWS service principals and authorized users within this account if the bucket has a public policy. Enabling this setting doesn't affect previously stored bucket policies, except that public and cross-account access within any public bucket policy, including non-public delegation to specific accounts, is blocked.
pabcRestrictPublicBuckets :: Lens' PublicAccessBlockConfiguration (Maybe Bool)
pabcRestrictPublicBuckets = lens _pabcRestrictPublicBuckets (\s a -> s {_pabcRestrictPublicBuckets = a})

-- | Specifies whether Amazon S3 should block public bucket policies for this bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls to PUT Bucket policy if the specified bucket policy allows public access.  Enabling this setting doesn't affect existing bucket policies.
pabcBlockPublicPolicy :: Lens' PublicAccessBlockConfiguration (Maybe Bool)
pabcBlockPublicPolicy = lens _pabcBlockPublicPolicy (\s a -> s {_pabcBlockPublicPolicy = a})

instance FromXML PublicAccessBlockConfiguration where
  parseXML x =
    PublicAccessBlockConfiguration'
      <$> (x .@? "IgnorePublicAcls")
      <*> (x .@? "BlockPublicAcls")
      <*> (x .@? "RestrictPublicBuckets")
      <*> (x .@? "BlockPublicPolicy")

instance Hashable PublicAccessBlockConfiguration

instance NFData PublicAccessBlockConfiguration

instance ToXML PublicAccessBlockConfiguration where
  toXML PublicAccessBlockConfiguration' {..} =
    mconcat
      [ "IgnorePublicAcls" @= _pabcIgnorePublicACLs,
        "BlockPublicAcls" @= _pabcBlockPublicACLs,
        "RestrictPublicBuckets" @= _pabcRestrictPublicBuckets,
        "BlockPublicPolicy" @= _pabcBlockPublicPolicy
      ]
