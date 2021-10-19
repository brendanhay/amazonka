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
import qualified Network.AWS.Prelude as Prelude

-- | An error or warning for a desired configuration option value.
--
-- /See:/ 'newValidationMessage' smart constructor.
data ValidationMessage = ValidationMessage'
  { -- | The name of the option.
    optionName :: Prelude.Maybe Prelude.Text,
    -- | An indication of the severity of this message:
    --
    -- -   @error@: This message indicates that this is not a valid setting for
    --     an option.
    --
    -- -   @warning@: This message is providing information you should take
    --     into account.
    severity :: Prelude.Maybe ValidationSeverity,
    -- | The namespace to which the option belongs.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | A message describing the error or warning.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'namespace', 'validationMessage_namespace' - The namespace to which the option belongs.
--
-- 'message', 'validationMessage_message' - A message describing the error or warning.
newValidationMessage ::
  ValidationMessage
newValidationMessage =
  ValidationMessage'
    { optionName = Prelude.Nothing,
      severity = Prelude.Nothing,
      namespace = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The name of the option.
validationMessage_optionName :: Lens.Lens' ValidationMessage (Prelude.Maybe Prelude.Text)
validationMessage_optionName = Lens.lens (\ValidationMessage' {optionName} -> optionName) (\s@ValidationMessage' {} a -> s {optionName = a} :: ValidationMessage)

-- | An indication of the severity of this message:
--
-- -   @error@: This message indicates that this is not a valid setting for
--     an option.
--
-- -   @warning@: This message is providing information you should take
--     into account.
validationMessage_severity :: Lens.Lens' ValidationMessage (Prelude.Maybe ValidationSeverity)
validationMessage_severity = Lens.lens (\ValidationMessage' {severity} -> severity) (\s@ValidationMessage' {} a -> s {severity = a} :: ValidationMessage)

-- | The namespace to which the option belongs.
validationMessage_namespace :: Lens.Lens' ValidationMessage (Prelude.Maybe Prelude.Text)
validationMessage_namespace = Lens.lens (\ValidationMessage' {namespace} -> namespace) (\s@ValidationMessage' {} a -> s {namespace = a} :: ValidationMessage)

-- | A message describing the error or warning.
validationMessage_message :: Lens.Lens' ValidationMessage (Prelude.Maybe Prelude.Text)
validationMessage_message = Lens.lens (\ValidationMessage' {message} -> message) (\s@ValidationMessage' {} a -> s {message = a} :: ValidationMessage)

instance Core.FromXML ValidationMessage where
  parseXML x =
    ValidationMessage'
      Prelude.<$> (x Core..@? "OptionName")
      Prelude.<*> (x Core..@? "Severity")
      Prelude.<*> (x Core..@? "Namespace")
      Prelude.<*> (x Core..@? "Message")

instance Prelude.Hashable ValidationMessage

instance Prelude.NFData ValidationMessage
