{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterHistory
  ( ParameterHistory (..),

    -- * Smart constructor
    mkParameterHistory,

    -- * Lenses
    phLastModifiedDate,
    phKeyId,
    phValue,
    phName,
    phTier,
    phVersion,
    phLastModifiedUser,
    phLabels,
    phAllowedPattern,
    phType,
    phDataType,
    phDescription,
    phPolicies,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ParameterInlinePolicy
import Network.AWS.SSM.Types.ParameterTier
import Network.AWS.SSM.Types.ParameterType

-- | Information about parameter usage.
--
-- /See:/ 'mkParameterHistory' smart constructor.
data ParameterHistory = ParameterHistory'
  { -- | Date the parameter was last changed or updated.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the query key used for this parameter.
    keyId :: Lude.Maybe Lude.Text,
    -- | The parameter value.
    value :: Lude.Maybe Lude.Text,
    -- | The name of the parameter.
    name :: Lude.Maybe Lude.Text,
    -- | The parameter tier.
    tier :: Lude.Maybe ParameterTier,
    -- | The parameter version.
    version :: Lude.Maybe Lude.Integer,
    -- | Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
    lastModifiedUser :: Lude.Maybe Lude.Text,
    -- | Labels assigned to the parameter version.
    labels :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | Parameter names can include the following letters and symbols.
    --
    -- a-zA-Z0-9_.-
    allowedPattern :: Lude.Maybe Lude.Text,
    -- | The type of parameter used.
    type' :: Lude.Maybe ParameterType,
    -- | The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
    dataType :: Lude.Maybe Lude.Text,
    -- | Information about the parameter.
    description :: Lude.Maybe Lude.Text,
    -- | Information about the policies assigned to a parameter.
    --
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> in the /AWS Systems Manager User Guide/ .
    policies :: Lude.Maybe [ParameterInlinePolicy]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterHistory' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - Date the parameter was last changed or updated.
-- * 'keyId' - The ID of the query key used for this parameter.
-- * 'value' - The parameter value.
-- * 'name' - The name of the parameter.
-- * 'tier' - The parameter tier.
-- * 'version' - The parameter version.
-- * 'lastModifiedUser' - Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
-- * 'labels' - Labels assigned to the parameter version.
-- * 'allowedPattern' - Parameter names can include the following letters and symbols.
--
-- a-zA-Z0-9_.-
-- * 'type'' - The type of parameter used.
-- * 'dataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
-- * 'description' - Information about the parameter.
-- * 'policies' - Information about the policies assigned to a parameter.
--
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> in the /AWS Systems Manager User Guide/ .
mkParameterHistory ::
  ParameterHistory
mkParameterHistory =
  ParameterHistory'
    { lastModifiedDate = Lude.Nothing,
      keyId = Lude.Nothing,
      value = Lude.Nothing,
      name = Lude.Nothing,
      tier = Lude.Nothing,
      version = Lude.Nothing,
      lastModifiedUser = Lude.Nothing,
      labels = Lude.Nothing,
      allowedPattern = Lude.Nothing,
      type' = Lude.Nothing,
      dataType = Lude.Nothing,
      description = Lude.Nothing,
      policies = Lude.Nothing
    }

-- | Date the parameter was last changed or updated.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phLastModifiedDate :: Lens.Lens' ParameterHistory (Lude.Maybe Lude.Timestamp)
phLastModifiedDate = Lens.lens (lastModifiedDate :: ParameterHistory -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: ParameterHistory)
{-# DEPRECATED phLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The ID of the query key used for this parameter.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phKeyId :: Lens.Lens' ParameterHistory (Lude.Maybe Lude.Text)
phKeyId = Lens.lens (keyId :: ParameterHistory -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: ParameterHistory)
{-# DEPRECATED phKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The parameter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phValue :: Lens.Lens' ParameterHistory (Lude.Maybe Lude.Text)
phValue = Lens.lens (value :: ParameterHistory -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ParameterHistory)
{-# DEPRECATED phValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phName :: Lens.Lens' ParameterHistory (Lude.Maybe Lude.Text)
phName = Lens.lens (name :: ParameterHistory -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ParameterHistory)
{-# DEPRECATED phName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The parameter tier.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phTier :: Lens.Lens' ParameterHistory (Lude.Maybe ParameterTier)
phTier = Lens.lens (tier :: ParameterHistory -> Lude.Maybe ParameterTier) (\s a -> s {tier = a} :: ParameterHistory)
{-# DEPRECATED phTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | The parameter version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phVersion :: Lens.Lens' ParameterHistory (Lude.Maybe Lude.Integer)
phVersion = Lens.lens (version :: ParameterHistory -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: ParameterHistory)
{-# DEPRECATED phVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
--
-- /Note:/ Consider using 'lastModifiedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phLastModifiedUser :: Lens.Lens' ParameterHistory (Lude.Maybe Lude.Text)
phLastModifiedUser = Lens.lens (lastModifiedUser :: ParameterHistory -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedUser = a} :: ParameterHistory)
{-# DEPRECATED phLastModifiedUser "Use generic-lens or generic-optics with 'lastModifiedUser' instead." #-}

-- | Labels assigned to the parameter version.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phLabels :: Lens.Lens' ParameterHistory (Lude.Maybe (Lude.NonEmpty Lude.Text))
phLabels = Lens.lens (labels :: ParameterHistory -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {labels = a} :: ParameterHistory)
{-# DEPRECATED phLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | Parameter names can include the following letters and symbols.
--
-- a-zA-Z0-9_.-
--
-- /Note:/ Consider using 'allowedPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phAllowedPattern :: Lens.Lens' ParameterHistory (Lude.Maybe Lude.Text)
phAllowedPattern = Lens.lens (allowedPattern :: ParameterHistory -> Lude.Maybe Lude.Text) (\s a -> s {allowedPattern = a} :: ParameterHistory)
{-# DEPRECATED phAllowedPattern "Use generic-lens or generic-optics with 'allowedPattern' instead." #-}

-- | The type of parameter used.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phType :: Lens.Lens' ParameterHistory (Lude.Maybe ParameterType)
phType = Lens.lens (type' :: ParameterHistory -> Lude.Maybe ParameterType) (\s a -> s {type' = a} :: ParameterHistory)
{-# DEPRECATED phType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phDataType :: Lens.Lens' ParameterHistory (Lude.Maybe Lude.Text)
phDataType = Lens.lens (dataType :: ParameterHistory -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: ParameterHistory)
{-# DEPRECATED phDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | Information about the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phDescription :: Lens.Lens' ParameterHistory (Lude.Maybe Lude.Text)
phDescription = Lens.lens (description :: ParameterHistory -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ParameterHistory)
{-# DEPRECATED phDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Information about the policies assigned to a parameter.
--
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phPolicies :: Lens.Lens' ParameterHistory (Lude.Maybe [ParameterInlinePolicy])
phPolicies = Lens.lens (policies :: ParameterHistory -> Lude.Maybe [ParameterInlinePolicy]) (\s a -> s {policies = a} :: ParameterHistory)
{-# DEPRECATED phPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

instance Lude.FromJSON ParameterHistory where
  parseJSON =
    Lude.withObject
      "ParameterHistory"
      ( \x ->
          ParameterHistory'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "KeyId")
            Lude.<*> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Tier")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "LastModifiedUser")
            Lude.<*> (x Lude..:? "Labels")
            Lude.<*> (x Lude..:? "AllowedPattern")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "DataType")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Policies" Lude..!= Lude.mempty)
      )
