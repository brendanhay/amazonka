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
    pksComment,
    pksId,
    pksName,
    pksCreatedTime,
    pksEncodedKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a public key.
--
-- /See:/ 'mkPublicKeySummary' smart constructor.
data PublicKeySummary = PublicKeySummary'
  { comment ::
      Lude.Maybe Lude.Text,
    id :: Lude.Text,
    name :: Lude.Text,
    createdTime :: Lude.ISO8601,
    encodedKey :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublicKeySummary' with the minimum fields required to make a request.
--
-- * 'comment' - A comment to describe the public key.
-- * 'createdTime' - The date and time when the public key was uploaded.
-- * 'encodedKey' - The public key.
-- * 'id' - The identifier of the public key.
-- * 'name' - A name to help identify the public key.
mkPublicKeySummary ::
  -- | 'id'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'createdTime'
  Lude.ISO8601 ->
  -- | 'encodedKey'
  Lude.Text ->
  PublicKeySummary
mkPublicKeySummary pId_ pName_ pCreatedTime_ pEncodedKey_ =
  PublicKeySummary'
    { comment = Lude.Nothing,
      id = pId_,
      name = pName_,
      createdTime = pCreatedTime_,
      encodedKey = pEncodedKey_
    }

-- | A comment to describe the public key.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksComment :: Lens.Lens' PublicKeySummary (Lude.Maybe Lude.Text)
pksComment = Lens.lens (comment :: PublicKeySummary -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: PublicKeySummary)
{-# DEPRECATED pksComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The identifier of the public key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksId :: Lens.Lens' PublicKeySummary Lude.Text
pksId = Lens.lens (id :: PublicKeySummary -> Lude.Text) (\s a -> s {id = a} :: PublicKeySummary)
{-# DEPRECATED pksId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A name to help identify the public key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksName :: Lens.Lens' PublicKeySummary Lude.Text
pksName = Lens.lens (name :: PublicKeySummary -> Lude.Text) (\s a -> s {name = a} :: PublicKeySummary)
{-# DEPRECATED pksName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time when the public key was uploaded.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksCreatedTime :: Lens.Lens' PublicKeySummary Lude.ISO8601
pksCreatedTime = Lens.lens (createdTime :: PublicKeySummary -> Lude.ISO8601) (\s a -> s {createdTime = a} :: PublicKeySummary)
{-# DEPRECATED pksCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The public key.
--
-- /Note:/ Consider using 'encodedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksEncodedKey :: Lens.Lens' PublicKeySummary Lude.Text
pksEncodedKey = Lens.lens (encodedKey :: PublicKeySummary -> Lude.Text) (\s a -> s {encodedKey = a} :: PublicKeySummary)
{-# DEPRECATED pksEncodedKey "Use generic-lens or generic-optics with 'encodedKey' instead." #-}

instance Lude.FromXML PublicKeySummary where
  parseXML x =
    PublicKeySummary'
      Lude.<$> (x Lude..@? "Comment")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "CreatedTime")
      Lude.<*> (x Lude..@ "EncodedKey")
