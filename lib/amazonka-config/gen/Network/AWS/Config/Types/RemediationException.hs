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
    reMessage,
    reExpirationTime,
    reConfigRuleName,
    reResourceType,
    reResourceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that represents the details about the remediation exception. The details include the rule name, an explanation of an exception, the time when the exception will be deleted, the resource ID, and resource type.
--
-- /See:/ 'mkRemediationException' smart constructor.
data RemediationException = RemediationException'
  { message ::
      Lude.Maybe Lude.Text,
    expirationTime :: Lude.Maybe Lude.Timestamp,
    configRuleName :: Lude.Text,
    resourceType :: Lude.Text,
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemediationException' with the minimum fields required to make a request.
--
-- * 'configRuleName' - The name of the AWS Config rule.
-- * 'expirationTime' - The time when the remediation exception will be deleted.
-- * 'message' - An explanation of an remediation exception.
-- * 'resourceId' - The ID of the resource (for example., sg-xxxxxx).
-- * 'resourceType' - The type of a resource.
mkRemediationException ::
  -- | 'configRuleName'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  RemediationException
mkRemediationException pConfigRuleName_ pResourceType_ pResourceId_ =
  RemediationException'
    { message = Lude.Nothing,
      expirationTime = Lude.Nothing,
      configRuleName = pConfigRuleName_,
      resourceType = pResourceType_,
      resourceId = pResourceId_
    }

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

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reConfigRuleName :: Lens.Lens' RemediationException Lude.Text
reConfigRuleName = Lens.lens (configRuleName :: RemediationException -> Lude.Text) (\s a -> s {configRuleName = a} :: RemediationException)
{-# DEPRECATED reConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The type of a resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reResourceType :: Lens.Lens' RemediationException Lude.Text
reResourceType = Lens.lens (resourceType :: RemediationException -> Lude.Text) (\s a -> s {resourceType = a} :: RemediationException)
{-# DEPRECATED reResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the resource (for example., sg-xxxxxx).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reResourceId :: Lens.Lens' RemediationException Lude.Text
reResourceId = Lens.lens (resourceId :: RemediationException -> Lude.Text) (\s a -> s {resourceId = a} :: RemediationException)
{-# DEPRECATED reResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.FromJSON RemediationException where
  parseJSON =
    Lude.withObject
      "RemediationException"
      ( \x ->
          RemediationException'
            Lude.<$> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "ExpirationTime")
            Lude.<*> (x Lude..: "ConfigRuleName")
            Lude.<*> (x Lude..: "ResourceType")
            Lude.<*> (x Lude..: "ResourceId")
      )
