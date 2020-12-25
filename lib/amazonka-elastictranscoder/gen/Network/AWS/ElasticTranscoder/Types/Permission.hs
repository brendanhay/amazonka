{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Permission
  ( Permission (..),

    -- * Smart constructor
    mkPermission,

    -- * Lenses
    pAccess,
    pGrantee,
    pGranteeType,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.AccessControl as Types
import qualified Network.AWS.ElasticTranscoder.Types.Grantee as Types
import qualified Network.AWS.ElasticTranscoder.Types.GranteeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @Permission@ structure.
--
-- /See:/ 'mkPermission' smart constructor.
data Permission = Permission'
  { -- | The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:
    --
    --
    --     * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --
    --     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --
    --     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --
    --     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
    access :: Core.Maybe [Types.AccessControl],
    -- | The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group.
    grantee :: Core.Maybe Types.Grantee,
    -- | The type of value that appears in the Grantee object:
    --
    --
    --     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.
    -- /Important:/ A canonical user ID is not the same as an AWS account number.
    --
    --
    --     * @Email@ : The registered email address of an AWS account.
    --
    --
    --     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
    granteeType :: Core.Maybe Types.GranteeType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Permission' value with any optional fields omitted.
mkPermission ::
  Permission
mkPermission =
  Permission'
    { access = Core.Nothing,
      grantee = Core.Nothing,
      granteeType = Core.Nothing
    }

-- | The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:
--
--
--     * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--
-- /Note:/ Consider using 'access' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAccess :: Lens.Lens' Permission (Core.Maybe [Types.AccessControl])
pAccess = Lens.field @"access"
{-# DEPRECATED pAccess "Use generic-lens or generic-optics with 'access' instead." #-}

-- | The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group.
--
-- /Note:/ Consider using 'grantee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGrantee :: Lens.Lens' Permission (Core.Maybe Types.Grantee)
pGrantee = Lens.field @"grantee"
{-# DEPRECATED pGrantee "Use generic-lens or generic-optics with 'grantee' instead." #-}

-- | The type of value that appears in the Grantee object:
--
--
--     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.
-- /Important:/ A canonical user ID is not the same as an AWS account number.
--
--
--     * @Email@ : The registered email address of an AWS account.
--
--
--     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
--
--
-- /Note:/ Consider using 'granteeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGranteeType :: Lens.Lens' Permission (Core.Maybe Types.GranteeType)
pGranteeType = Lens.field @"granteeType"
{-# DEPRECATED pGranteeType "Use generic-lens or generic-optics with 'granteeType' instead." #-}

instance Core.FromJSON Permission where
  toJSON Permission {..} =
    Core.object
      ( Core.catMaybes
          [ ("Access" Core..=) Core.<$> access,
            ("Grantee" Core..=) Core.<$> grantee,
            ("GranteeType" Core..=) Core.<$> granteeType
          ]
      )

instance Core.FromJSON Permission where
  parseJSON =
    Core.withObject "Permission" Core.$
      \x ->
        Permission'
          Core.<$> (x Core..:? "Access")
          Core.<*> (x Core..:? "Grantee")
          Core.<*> (x Core..:? "GranteeType")
