-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3DestinationAccessControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3DestinationAccessControl
  ( S3DestinationAccessControl (..),

    -- * Smart constructor
    mkS3DestinationAccessControl,

    -- * Lenses
    sdacCannedACL,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.S3ObjectCannedACL
import qualified Network.AWS.Prelude as Lude

-- | Optional. Have MediaConvert automatically apply Amazon S3 access control for the outputs in this output group. When you don't use this setting, S3 automatically applies the default access control list PRIVATE.
--
-- /See:/ 'mkS3DestinationAccessControl' smart constructor.
newtype S3DestinationAccessControl = S3DestinationAccessControl'
  { cannedACL ::
      Lude.Maybe S3ObjectCannedACL
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3DestinationAccessControl' with the minimum fields required to make a request.
--
-- * 'cannedACL' - Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
mkS3DestinationAccessControl ::
  S3DestinationAccessControl
mkS3DestinationAccessControl =
  S3DestinationAccessControl' {cannedACL = Lude.Nothing}

-- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
--
-- /Note:/ Consider using 'cannedACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdacCannedACL :: Lens.Lens' S3DestinationAccessControl (Lude.Maybe S3ObjectCannedACL)
sdacCannedACL = Lens.lens (cannedACL :: S3DestinationAccessControl -> Lude.Maybe S3ObjectCannedACL) (\s a -> s {cannedACL = a} :: S3DestinationAccessControl)
{-# DEPRECATED sdacCannedACL "Use generic-lens or generic-optics with 'cannedACL' instead." #-}

instance Lude.FromJSON S3DestinationAccessControl where
  parseJSON =
    Lude.withObject
      "S3DestinationAccessControl"
      ( \x ->
          S3DestinationAccessControl' Lude.<$> (x Lude..:? "cannedAcl")
      )

instance Lude.ToJSON S3DestinationAccessControl where
  toJSON S3DestinationAccessControl' {..} =
    Lude.object
      (Lude.catMaybes [("cannedAcl" Lude..=) Lude.<$> cannedACL])
