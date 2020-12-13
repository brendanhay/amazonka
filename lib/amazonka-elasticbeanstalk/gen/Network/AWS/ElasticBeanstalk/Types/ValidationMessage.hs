{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ValidationMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ValidationMessage
  ( ValidationMessage (..),

    -- * Smart constructor
    mkValidationMessage,

    -- * Lenses
    vmOptionName,
    vmSeverity,
    vmNamespace,
    vmMessage,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ValidationSeverity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An error or warning for a desired configuration option value.
--
-- /See:/ 'mkValidationMessage' smart constructor.
data ValidationMessage = ValidationMessage'
  { -- | The name of the option.
    optionName :: Lude.Maybe Lude.Text,
    -- | An indication of the severity of this message:
    --
    --
    --     * @error@ : This message indicates that this is not a valid setting for an option.
    --
    --
    --     * @warning@ : This message is providing information you should take into account.
    severity :: Lude.Maybe ValidationSeverity,
    -- | The namespace to which the option belongs.
    namespace :: Lude.Maybe Lude.Text,
    -- | A message describing the error or warning.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidationMessage' with the minimum fields required to make a request.
--
-- * 'optionName' - The name of the option.
-- * 'severity' - An indication of the severity of this message:
--
--
--     * @error@ : This message indicates that this is not a valid setting for an option.
--
--
--     * @warning@ : This message is providing information you should take into account.
--
--
-- * 'namespace' - The namespace to which the option belongs.
-- * 'message' - A message describing the error or warning.
mkValidationMessage ::
  ValidationMessage
mkValidationMessage =
  ValidationMessage'
    { optionName = Lude.Nothing,
      severity = Lude.Nothing,
      namespace = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The name of the option.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmOptionName :: Lens.Lens' ValidationMessage (Lude.Maybe Lude.Text)
vmOptionName = Lens.lens (optionName :: ValidationMessage -> Lude.Maybe Lude.Text) (\s a -> s {optionName = a} :: ValidationMessage)
{-# DEPRECATED vmOptionName "Use generic-lens or generic-optics with 'optionName' instead." #-}

-- | An indication of the severity of this message:
--
--
--     * @error@ : This message indicates that this is not a valid setting for an option.
--
--
--     * @warning@ : This message is providing information you should take into account.
--
--
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmSeverity :: Lens.Lens' ValidationMessage (Lude.Maybe ValidationSeverity)
vmSeverity = Lens.lens (severity :: ValidationMessage -> Lude.Maybe ValidationSeverity) (\s a -> s {severity = a} :: ValidationMessage)
{-# DEPRECATED vmSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The namespace to which the option belongs.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmNamespace :: Lens.Lens' ValidationMessage (Lude.Maybe Lude.Text)
vmNamespace = Lens.lens (namespace :: ValidationMessage -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: ValidationMessage)
{-# DEPRECATED vmNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | A message describing the error or warning.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmMessage :: Lens.Lens' ValidationMessage (Lude.Maybe Lude.Text)
vmMessage = Lens.lens (message :: ValidationMessage -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ValidationMessage)
{-# DEPRECATED vmMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML ValidationMessage where
  parseXML x =
    ValidationMessage'
      Lude.<$> (x Lude..@? "OptionName")
      Lude.<*> (x Lude..@? "Severity")
      Lude.<*> (x Lude..@? "Namespace")
      Lude.<*> (x Lude..@? "Message")
