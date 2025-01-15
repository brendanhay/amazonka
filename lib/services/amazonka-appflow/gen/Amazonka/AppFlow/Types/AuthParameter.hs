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
-- Module      : Amazonka.AppFlow.Types.AuthParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.AuthParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about required authentication parameters.
--
-- /See:/ 'newAuthParameter' smart constructor.
data AuthParameter = AuthParameter'
  { -- | Contains default values for this authentication parameter that are
    -- supplied by the connector.
    connectorSuppliedValues :: Prelude.Maybe [Prelude.Text],
    -- | A description about the authentication parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this authentication parameter is required.
    isRequired :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether this authentication parameter is a sensitive field.
    isSensitiveField :: Prelude.Maybe Prelude.Bool,
    -- | The authentication key required to authenticate with the connector.
    key :: Prelude.Maybe Prelude.Text,
    -- | Label used for authentication parameter.
    label :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorSuppliedValues', 'authParameter_connectorSuppliedValues' - Contains default values for this authentication parameter that are
-- supplied by the connector.
--
-- 'description', 'authParameter_description' - A description about the authentication parameter.
--
-- 'isRequired', 'authParameter_isRequired' - Indicates whether this authentication parameter is required.
--
-- 'isSensitiveField', 'authParameter_isSensitiveField' - Indicates whether this authentication parameter is a sensitive field.
--
-- 'key', 'authParameter_key' - The authentication key required to authenticate with the connector.
--
-- 'label', 'authParameter_label' - Label used for authentication parameter.
newAuthParameter ::
  AuthParameter
newAuthParameter =
  AuthParameter'
    { connectorSuppliedValues =
        Prelude.Nothing,
      description = Prelude.Nothing,
      isRequired = Prelude.Nothing,
      isSensitiveField = Prelude.Nothing,
      key = Prelude.Nothing,
      label = Prelude.Nothing
    }

-- | Contains default values for this authentication parameter that are
-- supplied by the connector.
authParameter_connectorSuppliedValues :: Lens.Lens' AuthParameter (Prelude.Maybe [Prelude.Text])
authParameter_connectorSuppliedValues = Lens.lens (\AuthParameter' {connectorSuppliedValues} -> connectorSuppliedValues) (\s@AuthParameter' {} a -> s {connectorSuppliedValues = a} :: AuthParameter) Prelude.. Lens.mapping Lens.coerced

-- | A description about the authentication parameter.
authParameter_description :: Lens.Lens' AuthParameter (Prelude.Maybe Prelude.Text)
authParameter_description = Lens.lens (\AuthParameter' {description} -> description) (\s@AuthParameter' {} a -> s {description = a} :: AuthParameter)

-- | Indicates whether this authentication parameter is required.
authParameter_isRequired :: Lens.Lens' AuthParameter (Prelude.Maybe Prelude.Bool)
authParameter_isRequired = Lens.lens (\AuthParameter' {isRequired} -> isRequired) (\s@AuthParameter' {} a -> s {isRequired = a} :: AuthParameter)

-- | Indicates whether this authentication parameter is a sensitive field.
authParameter_isSensitiveField :: Lens.Lens' AuthParameter (Prelude.Maybe Prelude.Bool)
authParameter_isSensitiveField = Lens.lens (\AuthParameter' {isSensitiveField} -> isSensitiveField) (\s@AuthParameter' {} a -> s {isSensitiveField = a} :: AuthParameter)

-- | The authentication key required to authenticate with the connector.
authParameter_key :: Lens.Lens' AuthParameter (Prelude.Maybe Prelude.Text)
authParameter_key = Lens.lens (\AuthParameter' {key} -> key) (\s@AuthParameter' {} a -> s {key = a} :: AuthParameter)

-- | Label used for authentication parameter.
authParameter_label :: Lens.Lens' AuthParameter (Prelude.Maybe Prelude.Text)
authParameter_label = Lens.lens (\AuthParameter' {label} -> label) (\s@AuthParameter' {} a -> s {label = a} :: AuthParameter)

instance Data.FromJSON AuthParameter where
  parseJSON =
    Data.withObject
      "AuthParameter"
      ( \x ->
          AuthParameter'
            Prelude.<$> ( x
                            Data..:? "connectorSuppliedValues"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "isRequired")
            Prelude.<*> (x Data..:? "isSensitiveField")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "label")
      )

instance Prelude.Hashable AuthParameter where
  hashWithSalt _salt AuthParameter' {..} =
    _salt
      `Prelude.hashWithSalt` connectorSuppliedValues
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isRequired
      `Prelude.hashWithSalt` isSensitiveField
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` label

instance Prelude.NFData AuthParameter where
  rnf AuthParameter' {..} =
    Prelude.rnf connectorSuppliedValues `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf isRequired `Prelude.seq`
          Prelude.rnf isSensitiveField `Prelude.seq`
            Prelude.rnf key `Prelude.seq`
              Prelude.rnf label
