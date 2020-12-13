{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroupConfig
  ( KeyGroupConfig (..),

    -- * Smart constructor
    mkKeyGroupConfig,

    -- * Lenses
    kgcItems,
    kgcName,
    kgcComment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A key group configuration.
--
-- A key group contains a list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies> .
--
-- /See:/ 'mkKeyGroupConfig' smart constructor.
data KeyGroupConfig = KeyGroupConfig'
  { -- | A list of the identifiers of the public keys in the key group.
    items :: [Lude.Text],
    -- | A name to identify the key group.
    name :: Lude.Text,
    -- | A comment to describe the key group.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyGroupConfig' with the minimum fields required to make a request.
--
-- * 'items' - A list of the identifiers of the public keys in the key group.
-- * 'name' - A name to identify the key group.
-- * 'comment' - A comment to describe the key group.
mkKeyGroupConfig ::
  -- | 'name'
  Lude.Text ->
  KeyGroupConfig
mkKeyGroupConfig pName_ =
  KeyGroupConfig'
    { items = Lude.mempty,
      name = pName_,
      comment = Lude.Nothing
    }

-- | A list of the identifiers of the public keys in the key group.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgcItems :: Lens.Lens' KeyGroupConfig [Lude.Text]
kgcItems = Lens.lens (items :: KeyGroupConfig -> [Lude.Text]) (\s a -> s {items = a} :: KeyGroupConfig)
{-# DEPRECATED kgcItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | A name to identify the key group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgcName :: Lens.Lens' KeyGroupConfig Lude.Text
kgcName = Lens.lens (name :: KeyGroupConfig -> Lude.Text) (\s a -> s {name = a} :: KeyGroupConfig)
{-# DEPRECATED kgcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A comment to describe the key group.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgcComment :: Lens.Lens' KeyGroupConfig (Lude.Maybe Lude.Text)
kgcComment = Lens.lens (comment :: KeyGroupConfig -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: KeyGroupConfig)
{-# DEPRECATED kgcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.FromXML KeyGroupConfig where
  parseXML x =
    KeyGroupConfig'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLList "PublicKey"
               )
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@? "Comment")

instance Lude.ToXML KeyGroupConfig where
  toXML KeyGroupConfig' {..} =
    Lude.mconcat
      [ "Items" Lude.@= Lude.toXMLList "PublicKey" items,
        "Name" Lude.@= name,
        "Comment" Lude.@= comment
      ]
