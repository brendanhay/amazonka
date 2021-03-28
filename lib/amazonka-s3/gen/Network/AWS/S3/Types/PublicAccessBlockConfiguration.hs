{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.PublicAccessBlockConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.PublicAccessBlockConfiguration
  ( PublicAccessBlockConfiguration (..)
  -- * Smart constructor
  , mkPublicAccessBlockConfiguration
  -- * Lenses
  , pabcBlockPublicAcls
  , pabcBlockPublicPolicy
  , pabcIgnorePublicAcls
  , pabcRestrictPublicBuckets
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | The PublicAccessBlock configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ . 
--
-- /See:/ 'mkPublicAccessBlockConfiguration' smart constructor.
data PublicAccessBlockConfiguration = PublicAccessBlockConfiguration'
  { blockPublicAcls :: Core.Maybe Core.Bool
    -- ^ Specifies whether Amazon S3 should block public access control lists (ACLs) for this bucket and objects in this bucket. Setting this element to @TRUE@ causes the following behavior:
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
  , blockPublicPolicy :: Core.Maybe Core.Bool
    -- ^ Specifies whether Amazon S3 should block public bucket policies for this bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls to PUT Bucket policy if the specified bucket policy allows public access. 
--
-- Enabling this setting doesn't affect existing bucket policies.
  , ignorePublicAcls :: Core.Maybe Core.Bool
    -- ^ Specifies whether Amazon S3 should ignore public ACLs for this bucket and objects in this bucket. Setting this element to @TRUE@ causes Amazon S3 to ignore all public ACLs on this bucket and objects in this bucket.
--
-- Enabling this setting doesn't affect the persistence of any existing ACLs and doesn't prevent new public ACLs from being set.
  , restrictPublicBuckets :: Core.Maybe Core.Bool
    -- ^ Specifies whether Amazon S3 should restrict public bucket policies for this bucket. Setting this element to @TRUE@ restricts access to this bucket to only AWS service principals and authorized users within this account if the bucket has a public policy.
--
-- Enabling this setting doesn't affect previously stored bucket policies, except that public and cross-account access within any public bucket policy, including non-public delegation to specific accounts, is blocked.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublicAccessBlockConfiguration' value with any optional fields omitted.
mkPublicAccessBlockConfiguration
    :: PublicAccessBlockConfiguration
mkPublicAccessBlockConfiguration
  = PublicAccessBlockConfiguration'{blockPublicAcls = Core.Nothing,
                                    blockPublicPolicy = Core.Nothing,
                                    ignorePublicAcls = Core.Nothing,
                                    restrictPublicBuckets = Core.Nothing}

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
-- /Note:/ Consider using 'blockPublicAcls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pabcBlockPublicAcls :: Lens.Lens' PublicAccessBlockConfiguration (Core.Maybe Core.Bool)
pabcBlockPublicAcls = Lens.field @"blockPublicAcls"
{-# INLINEABLE pabcBlockPublicAcls #-}
{-# DEPRECATED blockPublicAcls "Use generic-lens or generic-optics with 'blockPublicAcls' instead"  #-}

-- | Specifies whether Amazon S3 should block public bucket policies for this bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls to PUT Bucket policy if the specified bucket policy allows public access. 
--
-- Enabling this setting doesn't affect existing bucket policies.
--
-- /Note:/ Consider using 'blockPublicPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pabcBlockPublicPolicy :: Lens.Lens' PublicAccessBlockConfiguration (Core.Maybe Core.Bool)
pabcBlockPublicPolicy = Lens.field @"blockPublicPolicy"
{-# INLINEABLE pabcBlockPublicPolicy #-}
{-# DEPRECATED blockPublicPolicy "Use generic-lens or generic-optics with 'blockPublicPolicy' instead"  #-}

-- | Specifies whether Amazon S3 should ignore public ACLs for this bucket and objects in this bucket. Setting this element to @TRUE@ causes Amazon S3 to ignore all public ACLs on this bucket and objects in this bucket.
--
-- Enabling this setting doesn't affect the persistence of any existing ACLs and doesn't prevent new public ACLs from being set.
--
-- /Note:/ Consider using 'ignorePublicAcls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pabcIgnorePublicAcls :: Lens.Lens' PublicAccessBlockConfiguration (Core.Maybe Core.Bool)
pabcIgnorePublicAcls = Lens.field @"ignorePublicAcls"
{-# INLINEABLE pabcIgnorePublicAcls #-}
{-# DEPRECATED ignorePublicAcls "Use generic-lens or generic-optics with 'ignorePublicAcls' instead"  #-}

-- | Specifies whether Amazon S3 should restrict public bucket policies for this bucket. Setting this element to @TRUE@ restricts access to this bucket to only AWS service principals and authorized users within this account if the bucket has a public policy.
--
-- Enabling this setting doesn't affect previously stored bucket policies, except that public and cross-account access within any public bucket policy, including non-public delegation to specific accounts, is blocked.
--
-- /Note:/ Consider using 'restrictPublicBuckets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pabcRestrictPublicBuckets :: Lens.Lens' PublicAccessBlockConfiguration (Core.Maybe Core.Bool)
pabcRestrictPublicBuckets = Lens.field @"restrictPublicBuckets"
{-# INLINEABLE pabcRestrictPublicBuckets #-}
{-# DEPRECATED restrictPublicBuckets "Use generic-lens or generic-optics with 'restrictPublicBuckets' instead"  #-}

instance Core.ToXML PublicAccessBlockConfiguration where
        toXML PublicAccessBlockConfiguration{..}
          = Core.maybe Core.mempty (Core.toXMLElement "BlockPublicAcls")
              blockPublicAcls
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "BlockPublicPolicy")
                blockPublicPolicy
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "IgnorePublicAcls")
                ignorePublicAcls
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "RestrictPublicBuckets")
                restrictPublicBuckets

instance Core.FromXML PublicAccessBlockConfiguration where
        parseXML x
          = PublicAccessBlockConfiguration' Core.<$>
              (x Core..@? "BlockPublicAcls") Core.<*>
                x Core..@? "BlockPublicPolicy"
                Core.<*> x Core..@? "IgnorePublicAcls"
                Core.<*> x Core..@? "RestrictPublicBuckets"
