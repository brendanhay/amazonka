{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PublicKeySummary
  ( PublicKeySummary (..),

    -- * Smart constructor
    mkPublicKeySummary,

    -- * Lenses
    pksEncodedKey,
    pksCreatedTime,
    pksName,
    pksId,
    pksComment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a public key.
--
-- /See:/ 'mkPublicKeySummary' smart constructor.
data PublicKeySummary = PublicKeySummary'
  { -- | The public key.
    encodedKey :: Lude.Text,
    -- | The date and time when the public key was uploaded.
    createdTime :: Lude.DateTime,
    -- | A name to help identify the public key.
    name :: Lude.Text,
    -- | The identifier of the public key.
    id :: Lude.Text,
    -- | A comment to describe the public key.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublicKeySummary' with the minimum fields required to make a request.
--
-- * 'encodedKey' - The public key.
-- * 'createdTime' - The date and time when the public key was uploaded.
-- * 'name' - A name to help identify the public key.
-- * 'id' - The identifier of the public key.
-- * 'comment' - A comment to describe the public key.
mkPublicKeySummary ::
  -- | 'encodedKey'
  Lude.Text ->
  -- | 'createdTime'
  Lude.DateTime ->
  -- | 'name'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  PublicKeySummary
mkPublicKeySummary pEncodedKey_ pCreatedTime_ pName_ pId_ =
  PublicKeySummary'
    { encodedKey = pEncodedKey_,
      createdTime = pCreatedTime_,
      name = pName_,
      id = pId_,
      comment = Lude.Nothing
    }

-- | The public key.
--
-- /Note:/ Consider using 'encodedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksEncodedKey :: Lens.Lens' PublicKeySummary Lude.Text
pksEncodedKey = Lens.lens (encodedKey :: PublicKeySummary -> Lude.Text) (\s a -> s {encodedKey = a} :: PublicKeySummary)
{-# DEPRECATED pksEncodedKey "Use generic-lens or generic-optics with 'encodedKey' instead." #-}

-- | The date and time when the public key was uploaded.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksCreatedTime :: Lens.Lens' PublicKeySummary Lude.DateTime
pksCreatedTime = Lens.lens (createdTime :: PublicKeySummary -> Lude.DateTime) (\s a -> s {createdTime = a} :: PublicKeySummary)
{-# DEPRECATED pksCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | A name to help identify the public key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksName :: Lens.Lens' PublicKeySummary Lude.Text
pksName = Lens.lens (name :: PublicKeySummary -> Lude.Text) (\s a -> s {name = a} :: PublicKeySummary)
{-# DEPRECATED pksName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the public key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksId :: Lens.Lens' PublicKeySummary Lude.Text
pksId = Lens.lens (id :: PublicKeySummary -> Lude.Text) (\s a -> s {id = a} :: PublicKeySummary)
{-# DEPRECATED pksId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A comment to describe the public key.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksComment :: Lens.Lens' PublicKeySummary (Lude.Maybe Lude.Text)
pksComment = Lens.lens (comment :: PublicKeySummary -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: PublicKeySummary)
{-# DEPRECATED pksComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.FromXML PublicKeySummary where
  parseXML x =
    PublicKeySummary'
      Lude.<$> (x Lude..@ "EncodedKey")
      Lude.<*> (x Lude..@ "CreatedTime")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@? "Comment")
