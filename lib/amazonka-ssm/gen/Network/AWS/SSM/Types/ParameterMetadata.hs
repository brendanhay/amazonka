{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterMetadata
  ( ParameterMetadata (..),

    -- * Smart constructor
    mkParameterMetadata,

    -- * Lenses
    pmLastModifiedDate,
    pmKeyId,
    pmName,
    pmTier,
    pmVersion,
    pmLastModifiedUser,
    pmAllowedPattern,
    pmType,
    pmDataType,
    pmDescription,
    pmPolicies,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ParameterInlinePolicy
import Network.AWS.SSM.Types.ParameterTier
import Network.AWS.SSM.Types.ParameterType

-- | Metadata includes information like the ARN of the last user and the date/time the parameter was last used.
--
-- /See:/ 'mkParameterMetadata' smart constructor.
data ParameterMetadata = ParameterMetadata'
  { lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    keyId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    tier :: Lude.Maybe ParameterTier,
    version :: Lude.Maybe Lude.Integer,
    lastModifiedUser :: Lude.Maybe Lude.Text,
    allowedPattern :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ParameterType,
    dataType :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    policies :: Lude.Maybe [ParameterInlinePolicy]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterMetadata' with the minimum fields required to make a request.
--
-- * 'allowedPattern' - A parameter name can include only the following letters and symbols.
--
-- a-zA-Z0-9_.-
-- * 'dataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
-- * 'description' - Description of the parameter actions.
-- * 'keyId' - The ID of the query key used for this parameter.
-- * 'lastModifiedDate' - Date the parameter was last changed or updated.
-- * 'lastModifiedUser' - Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
-- * 'name' - The parameter name.
-- * 'policies' - A list of policies associated with a parameter.
-- * 'tier' - The parameter tier.
-- * 'type'' - The type of parameter. Valid parameter types include the following: @String@ , @StringList@ , and @SecureString@ .
-- * 'version' - The parameter version.
mkParameterMetadata ::
  ParameterMetadata
mkParameterMetadata =
  ParameterMetadata'
    { lastModifiedDate = Lude.Nothing,
      keyId = Lude.Nothing,
      name = Lude.Nothing,
      tier = Lude.Nothing,
      version = Lude.Nothing,
      lastModifiedUser = Lude.Nothing,
      allowedPattern = Lude.Nothing,
      type' = Lude.Nothing,
      dataType = Lude.Nothing,
      description = Lude.Nothing,
      policies = Lude.Nothing
    }

-- | Date the parameter was last changed or updated.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmLastModifiedDate :: Lens.Lens' ParameterMetadata (Lude.Maybe Lude.Timestamp)
pmLastModifiedDate = Lens.lens (lastModifiedDate :: ParameterMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: ParameterMetadata)
{-# DEPRECATED pmLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The ID of the query key used for this parameter.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmKeyId :: Lens.Lens' ParameterMetadata (Lude.Maybe Lude.Text)
pmKeyId = Lens.lens (keyId :: ParameterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: ParameterMetadata)
{-# DEPRECATED pmKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The parameter name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmName :: Lens.Lens' ParameterMetadata (Lude.Maybe Lude.Text)
pmName = Lens.lens (name :: ParameterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ParameterMetadata)
{-# DEPRECATED pmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The parameter tier.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmTier :: Lens.Lens' ParameterMetadata (Lude.Maybe ParameterTier)
pmTier = Lens.lens (tier :: ParameterMetadata -> Lude.Maybe ParameterTier) (\s a -> s {tier = a} :: ParameterMetadata)
{-# DEPRECATED pmTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | The parameter version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmVersion :: Lens.Lens' ParameterMetadata (Lude.Maybe Lude.Integer)
pmVersion = Lens.lens (version :: ParameterMetadata -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: ParameterMetadata)
{-# DEPRECATED pmVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
--
-- /Note:/ Consider using 'lastModifiedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmLastModifiedUser :: Lens.Lens' ParameterMetadata (Lude.Maybe Lude.Text)
pmLastModifiedUser = Lens.lens (lastModifiedUser :: ParameterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedUser = a} :: ParameterMetadata)
{-# DEPRECATED pmLastModifiedUser "Use generic-lens or generic-optics with 'lastModifiedUser' instead." #-}

-- | A parameter name can include only the following letters and symbols.
--
-- a-zA-Z0-9_.-
--
-- /Note:/ Consider using 'allowedPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmAllowedPattern :: Lens.Lens' ParameterMetadata (Lude.Maybe Lude.Text)
pmAllowedPattern = Lens.lens (allowedPattern :: ParameterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {allowedPattern = a} :: ParameterMetadata)
{-# DEPRECATED pmAllowedPattern "Use generic-lens or generic-optics with 'allowedPattern' instead." #-}

-- | The type of parameter. Valid parameter types include the following: @String@ , @StringList@ , and @SecureString@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmType :: Lens.Lens' ParameterMetadata (Lude.Maybe ParameterType)
pmType = Lens.lens (type' :: ParameterMetadata -> Lude.Maybe ParameterType) (\s a -> s {type' = a} :: ParameterMetadata)
{-# DEPRECATED pmType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmDataType :: Lens.Lens' ParameterMetadata (Lude.Maybe Lude.Text)
pmDataType = Lens.lens (dataType :: ParameterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: ParameterMetadata)
{-# DEPRECATED pmDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | Description of the parameter actions.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmDescription :: Lens.Lens' ParameterMetadata (Lude.Maybe Lude.Text)
pmDescription = Lens.lens (description :: ParameterMetadata -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ParameterMetadata)
{-# DEPRECATED pmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of policies associated with a parameter.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmPolicies :: Lens.Lens' ParameterMetadata (Lude.Maybe [ParameterInlinePolicy])
pmPolicies = Lens.lens (policies :: ParameterMetadata -> Lude.Maybe [ParameterInlinePolicy]) (\s a -> s {policies = a} :: ParameterMetadata)
{-# DEPRECATED pmPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

instance Lude.FromJSON ParameterMetadata where
  parseJSON =
    Lude.withObject
      "ParameterMetadata"
      ( \x ->
          ParameterMetadata'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "KeyId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Tier")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "LastModifiedUser")
            Lude.<*> (x Lude..:? "AllowedPattern")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "DataType")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Policies" Lude..!= Lude.mempty)
      )
