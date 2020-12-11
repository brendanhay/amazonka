-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.SourceAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.SourceAccessConfiguration
  ( SourceAccessConfiguration (..),

    -- * Smart constructor
    mkSourceAccessConfiguration,

    -- * Lenses
    sacURI,
    sacType,
  )
where

import Network.AWS.Lambda.Types.SourceAccessType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@
--
-- /See:/ 'mkSourceAccessConfiguration' smart constructor.
data SourceAccessConfiguration = SourceAccessConfiguration'
  { uri ::
      Lude.Maybe Lude.Text,
    type' :: Lude.Maybe SourceAccessType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceAccessConfiguration' with the minimum fields required to make a request.
--
-- * 'type'' - To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
--
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
-- * 'uri' - To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
--
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
mkSourceAccessConfiguration ::
  SourceAccessConfiguration
mkSourceAccessConfiguration =
  SourceAccessConfiguration'
    { uri = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
--
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sacURI :: Lens.Lens' SourceAccessConfiguration (Lude.Maybe Lude.Text)
sacURI = Lens.lens (uri :: SourceAccessConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {uri = a} :: SourceAccessConfiguration)
{-# DEPRECATED sacURI "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
--
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sacType :: Lens.Lens' SourceAccessConfiguration (Lude.Maybe SourceAccessType)
sacType = Lens.lens (type' :: SourceAccessConfiguration -> Lude.Maybe SourceAccessType) (\s a -> s {type' = a} :: SourceAccessConfiguration)
{-# DEPRECATED sacType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON SourceAccessConfiguration where
  parseJSON =
    Lude.withObject
      "SourceAccessConfiguration"
      ( \x ->
          SourceAccessConfiguration'
            Lude.<$> (x Lude..:? "URI") Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON SourceAccessConfiguration where
  toJSON SourceAccessConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [("URI" Lude..=) Lude.<$> uri, ("Type" Lude..=) Lude.<$> type']
      )
