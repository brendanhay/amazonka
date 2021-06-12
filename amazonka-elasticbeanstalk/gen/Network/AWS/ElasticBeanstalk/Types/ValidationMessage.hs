{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ValidationMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ValidationMessage where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.ValidationSeverity
import qualified Network.AWS.Lens as Lens

-- | An error or warning for a desired configuration option value.
--
-- /See:/ 'newValidationMessage' smart constructor.
data ValidationMessage = ValidationMessage'
  { -- | The name of the option.
    optionName :: Core.Maybe Core.Text,
    -- | An indication of the severity of this message:
    --
    -- -   @error@: This message indicates that this is not a valid setting for
    --     an option.
    --
    -- -   @warning@: This message is providing information you should take
    --     into account.
    severity :: Core.Maybe ValidationSeverity,
    -- | A message describing the error or warning.
    message :: Core.Maybe Core.Text,
    -- | The namespace to which the option belongs.
    namespace :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ValidationMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionName', 'validationMessage_optionName' - The name of the option.
--
-- 'severity', 'validationMessage_severity' - An indication of the severity of this message:
--
-- -   @error@: This message indicates that this is not a valid setting for
--     an option.
--
-- -   @warning@: This message is providing information you should take
--     into account.
--
-- 'message', 'validationMessage_message' - A message describing the error or warning.
--
-- 'namespace', 'validationMessage_namespace' - The namespace to which the option belongs.
newValidationMessage ::
  ValidationMessage
newValidationMessage =
  ValidationMessage'
    { optionName = Core.Nothing,
      severity = Core.Nothing,
      message = Core.Nothing,
      namespace = Core.Nothing
    }

-- | The name of the option.
validationMessage_optionName :: Lens.Lens' ValidationMessage (Core.Maybe Core.Text)
validationMessage_optionName = Lens.lens (\ValidationMessage' {optionName} -> optionName) (\s@ValidationMessage' {} a -> s {optionName = a} :: ValidationMessage)

-- | An indication of the severity of this message:
--
-- -   @error@: This message indicates that this is not a valid setting for
--     an option.
--
-- -   @warning@: This message is providing information you should take
--     into account.
validationMessage_severity :: Lens.Lens' ValidationMessage (Core.Maybe ValidationSeverity)
validationMessage_severity = Lens.lens (\ValidationMessage' {severity} -> severity) (\s@ValidationMessage' {} a -> s {severity = a} :: ValidationMessage)

-- | A message describing the error or warning.
validationMessage_message :: Lens.Lens' ValidationMessage (Core.Maybe Core.Text)
validationMessage_message = Lens.lens (\ValidationMessage' {message} -> message) (\s@ValidationMessage' {} a -> s {message = a} :: ValidationMessage)

-- | The namespace to which the option belongs.
validationMessage_namespace :: Lens.Lens' ValidationMessage (Core.Maybe Core.Text)
validationMessage_namespace = Lens.lens (\ValidationMessage' {namespace} -> namespace) (\s@ValidationMessage' {} a -> s {namespace = a} :: ValidationMessage)

instance Core.FromXML ValidationMessage where
  parseXML x =
    ValidationMessage'
      Core.<$> (x Core..@? "OptionName")
      Core.<*> (x Core..@? "Severity")
      Core.<*> (x Core..@? "Message")
      Core.<*> (x Core..@? "Namespace")

instance Core.Hashable ValidationMessage

instance Core.NFData ValidationMessage
