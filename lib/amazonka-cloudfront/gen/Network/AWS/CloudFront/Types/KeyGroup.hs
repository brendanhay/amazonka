{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroup
  ( KeyGroup (..),

    -- * Smart constructor
    mkKeyGroup,

    -- * Lenses
    kgId,
    kgLastModifiedTime,
    kgKeyGroupConfig,
  )
where

import Network.AWS.CloudFront.Types.KeyGroupConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A key group.
--
-- A key group contains a list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies> .
--
-- /See:/ 'mkKeyGroup' smart constructor.
data KeyGroup = KeyGroup'
  { id :: Lude.Text,
    lastModifiedTime :: Lude.DateTime,
    keyGroupConfig :: KeyGroupConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyGroup' with the minimum fields required to make a request.
--
-- * 'id' - The identifier for the key group.
-- * 'keyGroupConfig' - The key group configuration.
-- * 'lastModifiedTime' - The date and time when the key group was last modified.
mkKeyGroup ::
  -- | 'id'
  Lude.Text ->
  -- | 'lastModifiedTime'
  Lude.DateTime ->
  -- | 'keyGroupConfig'
  KeyGroupConfig ->
  KeyGroup
mkKeyGroup pId_ pLastModifiedTime_ pKeyGroupConfig_ =
  KeyGroup'
    { id = pId_,
      lastModifiedTime = pLastModifiedTime_,
      keyGroupConfig = pKeyGroupConfig_
    }

-- | The identifier for the key group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgId :: Lens.Lens' KeyGroup Lude.Text
kgId = Lens.lens (id :: KeyGroup -> Lude.Text) (\s a -> s {id = a} :: KeyGroup)
{-# DEPRECATED kgId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the key group was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgLastModifiedTime :: Lens.Lens' KeyGroup Lude.DateTime
kgLastModifiedTime = Lens.lens (lastModifiedTime :: KeyGroup -> Lude.DateTime) (\s a -> s {lastModifiedTime = a} :: KeyGroup)
{-# DEPRECATED kgLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The key group configuration.
--
-- /Note:/ Consider using 'keyGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgKeyGroupConfig :: Lens.Lens' KeyGroup KeyGroupConfig
kgKeyGroupConfig = Lens.lens (keyGroupConfig :: KeyGroup -> KeyGroupConfig) (\s a -> s {keyGroupConfig = a} :: KeyGroup)
{-# DEPRECATED kgKeyGroupConfig "Use generic-lens or generic-optics with 'keyGroupConfig' instead." #-}

instance Lude.FromXML KeyGroup where
  parseXML x =
    KeyGroup'
      Lude.<$> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "KeyGroupConfig")
