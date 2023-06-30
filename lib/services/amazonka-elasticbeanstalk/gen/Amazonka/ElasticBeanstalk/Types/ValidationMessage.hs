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
-- Module      : Amazonka.ElasticBeanstalk.Types.ValidationMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ValidationMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.ValidationSeverity
import qualified Amazonka.Prelude as Prelude

-- | An error or warning for a desired configuration option value.
--
-- /See:/ 'newValidationMessage' smart constructor.
data ValidationMessage = ValidationMessage'
  { -- | A message describing the error or warning.
    message :: Prelude.Maybe Prelude.Text,
    -- | The namespace to which the option belongs.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the option.
    optionName :: Prelude.Maybe Prelude.Text,
    -- | An indication of the severity of this message:
    --
    -- -   @error@: This message indicates that this is not a valid setting for
    --     an option.
    --
    -- -   @warning@: This message is providing information you should take
    --     into account.
    severity :: Prelude.Maybe ValidationSeverity
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
-- 'message', 'validationMessage_message' - A message describing the error or warning.
--
-- 'namespace', 'validationMessage_namespace' - The namespace to which the option belongs.
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
newValidationMessage ::
  ValidationMessage
newValidationMessage =
  ValidationMessage'
    { message = Prelude.Nothing,
      namespace = Prelude.Nothing,
      optionName = Prelude.Nothing,
      severity = Prelude.Nothing
    }

-- | A message describing the error or warning.
validationMessage_message :: Lens.Lens' ValidationMessage (Prelude.Maybe Prelude.Text)
validationMessage_message = Lens.lens (\ValidationMessage' {message} -> message) (\s@ValidationMessage' {} a -> s {message = a} :: ValidationMessage)

-- | The namespace to which the option belongs.
validationMessage_namespace :: Lens.Lens' ValidationMessage (Prelude.Maybe Prelude.Text)
validationMessage_namespace = Lens.lens (\ValidationMessage' {namespace} -> namespace) (\s@ValidationMessage' {} a -> s {namespace = a} :: ValidationMessage)

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

instance Data.FromXML ValidationMessage where
  parseXML x =
    ValidationMessage'
      Prelude.<$> (x Data..@? "Message")
      Prelude.<*> (x Data..@? "Namespace")
      Prelude.<*> (x Data..@? "OptionName")
      Prelude.<*> (x Data..@? "Severity")

instance Prelude.Hashable ValidationMessage where
  hashWithSalt _salt ValidationMessage' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` optionName
      `Prelude.hashWithSalt` severity

instance Prelude.NFData ValidationMessage where
  rnf ValidationMessage' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf optionName
      `Prelude.seq` Prelude.rnf severity
