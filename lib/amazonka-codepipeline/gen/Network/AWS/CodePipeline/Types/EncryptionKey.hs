{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.EncryptionKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.EncryptionKey
  ( EncryptionKey (..),

    -- * Smart constructor
    mkEncryptionKey,

    -- * Lenses
    ekId,
    ekType,
  )
where

import Network.AWS.CodePipeline.Types.EncryptionKeyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.
--
-- /See:/ 'mkEncryptionKey' smart constructor.
data EncryptionKey = EncryptionKey'
  { id :: Lude.Text,
    type' :: EncryptionKeyType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionKey' with the minimum fields required to make a request.
--
-- * 'id' - The ID used to identify the key. For an AWS KMS key, you can use the key ID, the key ARN, or the alias ARN.
-- * 'type'' - The type of encryption key, such as an AWS Key Management Service (AWS KMS) key. When creating or updating a pipeline, the value must be set to 'KMS'.
mkEncryptionKey ::
  -- | 'id'
  Lude.Text ->
  -- | 'type''
  EncryptionKeyType ->
  EncryptionKey
mkEncryptionKey pId_ pType_ =
  EncryptionKey' {id = pId_, type' = pType_}

-- | The ID used to identify the key. For an AWS KMS key, you can use the key ID, the key ARN, or the alias ARN.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ekId :: Lens.Lens' EncryptionKey Lude.Text
ekId = Lens.lens (id :: EncryptionKey -> Lude.Text) (\s a -> s {id = a} :: EncryptionKey)
{-# DEPRECATED ekId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of encryption key, such as an AWS Key Management Service (AWS KMS) key. When creating or updating a pipeline, the value must be set to 'KMS'.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ekType :: Lens.Lens' EncryptionKey EncryptionKeyType
ekType = Lens.lens (type' :: EncryptionKey -> EncryptionKeyType) (\s a -> s {type' = a} :: EncryptionKey)
{-# DEPRECATED ekType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON EncryptionKey where
  parseJSON =
    Lude.withObject
      "EncryptionKey"
      ( \x ->
          EncryptionKey'
            Lude.<$> (x Lude..: "id") Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON EncryptionKey where
  toJSON EncryptionKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("id" Lude..= id), Lude.Just ("type" Lude..= type')]
      )
