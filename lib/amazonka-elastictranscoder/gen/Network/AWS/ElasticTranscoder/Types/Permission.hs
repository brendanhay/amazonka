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
    pGranteeType,
    pGrantee,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @Permission@ structure.
--
-- /See:/ 'mkPermission' smart constructor.
data Permission = Permission'
  { access :: Lude.Maybe [Lude.Text],
    granteeType :: Lude.Maybe Lude.Text,
    grantee :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- * 'access' - The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:
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
-- * 'grantee' - The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group.
-- * 'granteeType' - The type of value that appears in the Grantee object:
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
mkPermission ::
  Permission
mkPermission =
  Permission'
    { access = Lude.Nothing,
      granteeType = Lude.Nothing,
      grantee = Lude.Nothing
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
pAccess :: Lens.Lens' Permission (Lude.Maybe [Lude.Text])
pAccess = Lens.lens (access :: Permission -> Lude.Maybe [Lude.Text]) (\s a -> s {access = a} :: Permission)
{-# DEPRECATED pAccess "Use generic-lens or generic-optics with 'access' instead." #-}

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
pGranteeType :: Lens.Lens' Permission (Lude.Maybe Lude.Text)
pGranteeType = Lens.lens (granteeType :: Permission -> Lude.Maybe Lude.Text) (\s a -> s {granteeType = a} :: Permission)
{-# DEPRECATED pGranteeType "Use generic-lens or generic-optics with 'granteeType' instead." #-}

-- | The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group.
--
-- /Note:/ Consider using 'grantee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGrantee :: Lens.Lens' Permission (Lude.Maybe Lude.Text)
pGrantee = Lens.lens (grantee :: Permission -> Lude.Maybe Lude.Text) (\s a -> s {grantee = a} :: Permission)
{-# DEPRECATED pGrantee "Use generic-lens or generic-optics with 'grantee' instead." #-}

instance Lude.FromJSON Permission where
  parseJSON =
    Lude.withObject
      "Permission"
      ( \x ->
          Permission'
            Lude.<$> (x Lude..:? "Access" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "GranteeType")
            Lude.<*> (x Lude..:? "Grantee")
      )

instance Lude.ToJSON Permission where
  toJSON Permission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Access" Lude..=) Lude.<$> access,
            ("GranteeType" Lude..=) Lude.<$> granteeType,
            ("Grantee" Lude..=) Lude.<$> grantee
          ]
      )
