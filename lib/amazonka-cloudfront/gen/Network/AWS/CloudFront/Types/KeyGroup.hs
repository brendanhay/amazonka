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
    kgKeyGroupConfig,
    kgLastModifiedTime,
    kgId,
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
  { -- | The key group configuration.
    keyGroupConfig :: KeyGroupConfig,
    -- | The date and time when the key group was last modified.
    lastModifiedTime :: Lude.DateTime,
    -- | The identifier for the key group.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyGroup' with the minimum fields required to make a request.
--
-- * 'keyGroupConfig' - The key group configuration.
-- * 'lastModifiedTime' - The date and time when the key group was last modified.
-- * 'id' - The identifier for the key group.
mkKeyGroup ::
  -- | 'keyGroupConfig'
  KeyGroupConfig ->
  -- | 'lastModifiedTime'
  Lude.DateTime ->
  -- | 'id'
  Lude.Text ->
  KeyGroup
mkKeyGroup pKeyGroupConfig_ pLastModifiedTime_ pId_ =
  KeyGroup'
    { keyGroupConfig = pKeyGroupConfig_,
      lastModifiedTime = pLastModifiedTime_,
      id = pId_
    }

-- | The key group configuration.
--
-- /Note:/ Consider using 'keyGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgKeyGroupConfig :: Lens.Lens' KeyGroup KeyGroupConfig
kgKeyGroupConfig = Lens.lens (keyGroupConfig :: KeyGroup -> KeyGroupConfig) (\s a -> s {keyGroupConfig = a} :: KeyGroup)
{-# DEPRECATED kgKeyGroupConfig "Use generic-lens or generic-optics with 'keyGroupConfig' instead." #-}

-- | The date and time when the key group was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgLastModifiedTime :: Lens.Lens' KeyGroup Lude.DateTime
kgLastModifiedTime = Lens.lens (lastModifiedTime :: KeyGroup -> Lude.DateTime) (\s a -> s {lastModifiedTime = a} :: KeyGroup)
{-# DEPRECATED kgLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The identifier for the key group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgId :: Lens.Lens' KeyGroup Lude.Text
kgId = Lens.lens (id :: KeyGroup -> Lude.Text) (\s a -> s {id = a} :: KeyGroup)
{-# DEPRECATED kgId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML KeyGroup where
  parseXML x =
    KeyGroup'
      Lude.<$> (x Lude..@ "KeyGroupConfig")
      Lude.<*> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "Id")
