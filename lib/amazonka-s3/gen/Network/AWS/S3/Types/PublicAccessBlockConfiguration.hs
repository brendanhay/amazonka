-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.PublicAccessBlockConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.PublicAccessBlockConfiguration
  ( PublicAccessBlockConfiguration (..),

    -- * Smart constructor
    mkPublicAccessBlockConfiguration,

    -- * Lenses
    pabcIgnorePublicACLs,
    pabcBlockPublicACLs,
    pabcRestrictPublicBuckets,
    pabcBlockPublicPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | The PublicAccessBlock configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkPublicAccessBlockConfiguration' smart constructor.
data PublicAccessBlockConfiguration = PublicAccessBlockConfiguration'
  { ignorePublicACLs ::
      Lude.Maybe Lude.Bool,
    blockPublicACLs ::
      Lude.Maybe Lude.Bool,
    restrictPublicBuckets ::
      Lude.Maybe Lude.Bool,
    blockPublicPolicy ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublicAccessBlockConfiguration' with the minimum fields required to make a request.
--
-- * 'blockPublicACLs' - Specifies whether Amazon S3 should block public access control lists (ACLs) for this bucket and objects in this bucket. Setting this element to @TRUE@ causes the following behavior:
--
--
--     * PUT Bucket acl and PUT Object acl calls fail if the specified ACL is public.
--
--
--     * PUT Object calls fail if the request includes a public ACL.
--
--
--     * PUT Bucket calls fail if the request includes a public ACL.
--
--
-- Enabling this setting doesn't affect existing policies or ACLs.
-- * 'blockPublicPolicy' - Specifies whether Amazon S3 should block public bucket policies for this bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls to PUT Bucket policy if the specified bucket policy allows public access.
--
-- Enabling this setting doesn't affect existing bucket policies.
-- * 'ignorePublicACLs' - Specifies whether Amazon S3 should ignore public ACLs for this bucket and objects in this bucket. Setting this element to @TRUE@ causes Amazon S3 to ignore all public ACLs on this bucket and objects in this bucket.
--
-- Enabling this setting doesn't affect the persistence of any existing ACLs and doesn't prevent new public ACLs from being set.
-- * 'restrictPublicBuckets' - Specifies whether Amazon S3 should restrict public bucket policies for this bucket. Setting this element to @TRUE@ restricts access to this bucket to only AWS service principals and authorized users within this account if the bucket has a public policy.
--
-- Enabling this setting doesn't affect previously stored bucket policies, except that public and cross-account access within any public bucket policy, including non-public delegation to specific accounts, is blocked.
mkPublicAccessBlockConfiguration ::
  PublicAccessBlockConfiguration
mkPublicAccessBlockConfiguration =
  PublicAccessBlockConfiguration'
    { ignorePublicACLs = Lude.Nothing,
      blockPublicACLs = Lude.Nothing,
      restrictPublicBuckets = Lude.Nothing,
      blockPublicPolicy = Lude.Nothing
    }

-- | Specifies whether Amazon S3 should ignore public ACLs for this bucket and objects in this bucket. Setting this element to @TRUE@ causes Amazon S3 to ignore all public ACLs on this bucket and objects in this bucket.
--
-- Enabling this setting doesn't affect the persistence of any existing ACLs and doesn't prevent new public ACLs from being set.
--
-- /Note:/ Consider using 'ignorePublicACLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pabcIgnorePublicACLs :: Lens.Lens' PublicAccessBlockConfiguration (Lude.Maybe Lude.Bool)
pabcIgnorePublicACLs = Lens.lens (ignorePublicACLs :: PublicAccessBlockConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {ignorePublicACLs = a} :: PublicAccessBlockConfiguration)
{-# DEPRECATED pabcIgnorePublicACLs "Use generic-lens or generic-optics with 'ignorePublicACLs' instead." #-}

-- | Specifies whether Amazon S3 should block public access control lists (ACLs) for this bucket and objects in this bucket. Setting this element to @TRUE@ causes the following behavior:
--
--
--     * PUT Bucket acl and PUT Object acl calls fail if the specified ACL is public.
--
--
--     * PUT Object calls fail if the request includes a public ACL.
--
--
--     * PUT Bucket calls fail if the request includes a public ACL.
--
--
-- Enabling this setting doesn't affect existing policies or ACLs.
--
-- /Note:/ Consider using 'blockPublicACLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pabcBlockPublicACLs :: Lens.Lens' PublicAccessBlockConfiguration (Lude.Maybe Lude.Bool)
pabcBlockPublicACLs = Lens.lens (blockPublicACLs :: PublicAccessBlockConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {blockPublicACLs = a} :: PublicAccessBlockConfiguration)
{-# DEPRECATED pabcBlockPublicACLs "Use generic-lens or generic-optics with 'blockPublicACLs' instead." #-}

-- | Specifies whether Amazon S3 should restrict public bucket policies for this bucket. Setting this element to @TRUE@ restricts access to this bucket to only AWS service principals and authorized users within this account if the bucket has a public policy.
--
-- Enabling this setting doesn't affect previously stored bucket policies, except that public and cross-account access within any public bucket policy, including non-public delegation to specific accounts, is blocked.
--
-- /Note:/ Consider using 'restrictPublicBuckets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pabcRestrictPublicBuckets :: Lens.Lens' PublicAccessBlockConfiguration (Lude.Maybe Lude.Bool)
pabcRestrictPublicBuckets = Lens.lens (restrictPublicBuckets :: PublicAccessBlockConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {restrictPublicBuckets = a} :: PublicAccessBlockConfiguration)
{-# DEPRECATED pabcRestrictPublicBuckets "Use generic-lens or generic-optics with 'restrictPublicBuckets' instead." #-}

-- | Specifies whether Amazon S3 should block public bucket policies for this bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls to PUT Bucket policy if the specified bucket policy allows public access.
--
-- Enabling this setting doesn't affect existing bucket policies.
--
-- /Note:/ Consider using 'blockPublicPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pabcBlockPublicPolicy :: Lens.Lens' PublicAccessBlockConfiguration (Lude.Maybe Lude.Bool)
pabcBlockPublicPolicy = Lens.lens (blockPublicPolicy :: PublicAccessBlockConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {blockPublicPolicy = a} :: PublicAccessBlockConfiguration)
{-# DEPRECATED pabcBlockPublicPolicy "Use generic-lens or generic-optics with 'blockPublicPolicy' instead." #-}

instance Lude.FromXML PublicAccessBlockConfiguration where
  parseXML x =
    PublicAccessBlockConfiguration'
      Lude.<$> (x Lude..@? "IgnorePublicAcls")
      Lude.<*> (x Lude..@? "BlockPublicAcls")
      Lude.<*> (x Lude..@? "RestrictPublicBuckets")
      Lude.<*> (x Lude..@? "BlockPublicPolicy")

instance Lude.ToXML PublicAccessBlockConfiguration where
  toXML PublicAccessBlockConfiguration' {..} =
    Lude.mconcat
      [ "IgnorePublicAcls" Lude.@= ignorePublicACLs,
        "BlockPublicAcls" Lude.@= blockPublicACLs,
        "RestrictPublicBuckets" Lude.@= restrictPublicBuckets,
        "BlockPublicPolicy" Lude.@= blockPublicPolicy
      ]
