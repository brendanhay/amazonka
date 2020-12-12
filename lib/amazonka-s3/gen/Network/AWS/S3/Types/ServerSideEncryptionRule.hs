{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryptionRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ServerSideEncryptionRule
  ( ServerSideEncryptionRule (..),

    -- * Smart constructor
    mkServerSideEncryptionRule,

    -- * Lenses
    sserApplyServerSideEncryptionByDefault,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ServerSideEncryptionByDefault

-- | Specifies the default server-side encryption configuration.
--
-- /See:/ 'mkServerSideEncryptionRule' smart constructor.
newtype ServerSideEncryptionRule = ServerSideEncryptionRule'
  { applyServerSideEncryptionByDefault ::
      Lude.Maybe
        ServerSideEncryptionByDefault
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerSideEncryptionRule' with the minimum fields required to make a request.
--
-- * 'applyServerSideEncryptionByDefault' - Specifies the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied.
mkServerSideEncryptionRule ::
  ServerSideEncryptionRule
mkServerSideEncryptionRule =
  ServerSideEncryptionRule'
    { applyServerSideEncryptionByDefault =
        Lude.Nothing
    }

-- | Specifies the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied.
--
-- /Note:/ Consider using 'applyServerSideEncryptionByDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserApplyServerSideEncryptionByDefault :: Lens.Lens' ServerSideEncryptionRule (Lude.Maybe ServerSideEncryptionByDefault)
sserApplyServerSideEncryptionByDefault = Lens.lens (applyServerSideEncryptionByDefault :: ServerSideEncryptionRule -> Lude.Maybe ServerSideEncryptionByDefault) (\s a -> s {applyServerSideEncryptionByDefault = a} :: ServerSideEncryptionRule)
{-# DEPRECATED sserApplyServerSideEncryptionByDefault "Use generic-lens or generic-optics with 'applyServerSideEncryptionByDefault' instead." #-}

instance Lude.FromXML ServerSideEncryptionRule where
  parseXML x =
    ServerSideEncryptionRule'
      Lude.<$> (x Lude..@? "ApplyServerSideEncryptionByDefault")

instance Lude.ToXML ServerSideEncryptionRule where
  toXML ServerSideEncryptionRule' {..} =
    Lude.mconcat
      [ "ApplyServerSideEncryptionByDefault"
          Lude.@= applyServerSideEncryptionByDefault
      ]
