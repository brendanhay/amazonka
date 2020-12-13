{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationException
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationException
  ( RemediationException (..),

    -- * Smart constructor
    mkRemediationException,

    -- * Lenses
    reResourceId,
    reResourceType,
    reConfigRuleName,
    reMessage,
    reExpirationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that represents the details about the remediation exception. The details include the rule name, an explanation of an exception, the time when the exception will be deleted, the resource ID, and resource type.
--
-- /See:/ 'mkRemediationException' smart constructor.
data RemediationException = RemediationException'
  { -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Lude.Text,
    -- | The type of a resource.
    resourceType :: Lude.Text,
    -- | The name of the AWS Config rule.
    configRuleName :: Lude.Text,
    -- | An explanation of an remediation exception.
    message :: Lude.Maybe Lude.Text,
    -- | The time when the remediation exception will be deleted.
    expirationTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemediationException' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource (for example., sg-xxxxxx).
-- * 'resourceType' - The type of a resource.
-- * 'configRuleName' - The name of the AWS Config rule.
-- * 'message' - An explanation of an remediation exception.
-- * 'expirationTime' - The time when the remediation exception will be deleted.
mkRemediationException ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'configRuleName'
  Lude.Text ->
  RemediationException
mkRemediationException pResourceId_ pResourceType_ pConfigRuleName_ =
  RemediationException'
    { resourceId = pResourceId_,
      resourceType = pResourceType_,
      configRuleName = pConfigRuleName_,
      message = Lude.Nothing,
      expirationTime = Lude.Nothing
    }

-- | The ID of the resource (for example., sg-xxxxxx).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reResourceId :: Lens.Lens' RemediationException Lude.Text
reResourceId = Lens.lens (resourceId :: RemediationException -> Lude.Text) (\s a -> s {resourceId = a} :: RemediationException)
{-# DEPRECATED reResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of a resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reResourceType :: Lens.Lens' RemediationException Lude.Text
reResourceType = Lens.lens (resourceType :: RemediationException -> Lude.Text) (\s a -> s {resourceType = a} :: RemediationException)
{-# DEPRECATED reResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reConfigRuleName :: Lens.Lens' RemediationException Lude.Text
reConfigRuleName = Lens.lens (configRuleName :: RemediationException -> Lude.Text) (\s a -> s {configRuleName = a} :: RemediationException)
{-# DEPRECATED reConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | An explanation of an remediation exception.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reMessage :: Lens.Lens' RemediationException (Lude.Maybe Lude.Text)
reMessage = Lens.lens (message :: RemediationException -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: RemediationException)
{-# DEPRECATED reMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time when the remediation exception will be deleted.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reExpirationTime :: Lens.Lens' RemediationException (Lude.Maybe Lude.Timestamp)
reExpirationTime = Lens.lens (expirationTime :: RemediationException -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationTime = a} :: RemediationException)
{-# DEPRECATED reExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

instance Lude.FromJSON RemediationException where
  parseJSON =
    Lude.withObject
      "RemediationException"
      ( \x ->
          RemediationException'
            Lude.<$> (x Lude..: "ResourceId")
            Lude.<*> (x Lude..: "ResourceType")
            Lude.<*> (x Lude..: "ConfigRuleName")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "ExpirationTime")
      )
