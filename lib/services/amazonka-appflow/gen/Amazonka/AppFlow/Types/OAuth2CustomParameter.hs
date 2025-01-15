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
-- Module      : Amazonka.AppFlow.Types.OAuth2CustomParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.OAuth2CustomParameter where

import Amazonka.AppFlow.Types.OAuth2CustomPropType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Custom parameter required for OAuth 2.0 authentication.
--
-- /See:/ 'newOAuth2CustomParameter' smart constructor.
data OAuth2CustomParameter = OAuth2CustomParameter'
  { -- | Contains default values for this authentication parameter that are
    -- supplied by the connector.
    connectorSuppliedValues :: Prelude.Maybe [Prelude.Text],
    -- | A description about the custom parameter used for OAuth 2.0
    -- authentication.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the custom parameter for OAuth 2.0 authentication is
    -- required.
    isRequired :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether this authentication custom parameter is a sensitive
    -- field.
    isSensitiveField :: Prelude.Maybe Prelude.Bool,
    -- | The key of the custom parameter required for OAuth 2.0 authentication.
    key :: Prelude.Maybe Prelude.Text,
    -- | The label of the custom parameter used for OAuth 2.0 authentication.
    label :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether custom parameter is used with TokenUrl or AuthUrl.
    type' :: Prelude.Maybe OAuth2CustomPropType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OAuth2CustomParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorSuppliedValues', 'oAuth2CustomParameter_connectorSuppliedValues' - Contains default values for this authentication parameter that are
-- supplied by the connector.
--
-- 'description', 'oAuth2CustomParameter_description' - A description about the custom parameter used for OAuth 2.0
-- authentication.
--
-- 'isRequired', 'oAuth2CustomParameter_isRequired' - Indicates whether the custom parameter for OAuth 2.0 authentication is
-- required.
--
-- 'isSensitiveField', 'oAuth2CustomParameter_isSensitiveField' - Indicates whether this authentication custom parameter is a sensitive
-- field.
--
-- 'key', 'oAuth2CustomParameter_key' - The key of the custom parameter required for OAuth 2.0 authentication.
--
-- 'label', 'oAuth2CustomParameter_label' - The label of the custom parameter used for OAuth 2.0 authentication.
--
-- 'type'', 'oAuth2CustomParameter_type' - Indicates whether custom parameter is used with TokenUrl or AuthUrl.
newOAuth2CustomParameter ::
  OAuth2CustomParameter
newOAuth2CustomParameter =
  OAuth2CustomParameter'
    { connectorSuppliedValues =
        Prelude.Nothing,
      description = Prelude.Nothing,
      isRequired = Prelude.Nothing,
      isSensitiveField = Prelude.Nothing,
      key = Prelude.Nothing,
      label = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Contains default values for this authentication parameter that are
-- supplied by the connector.
oAuth2CustomParameter_connectorSuppliedValues :: Lens.Lens' OAuth2CustomParameter (Prelude.Maybe [Prelude.Text])
oAuth2CustomParameter_connectorSuppliedValues = Lens.lens (\OAuth2CustomParameter' {connectorSuppliedValues} -> connectorSuppliedValues) (\s@OAuth2CustomParameter' {} a -> s {connectorSuppliedValues = a} :: OAuth2CustomParameter) Prelude.. Lens.mapping Lens.coerced

-- | A description about the custom parameter used for OAuth 2.0
-- authentication.
oAuth2CustomParameter_description :: Lens.Lens' OAuth2CustomParameter (Prelude.Maybe Prelude.Text)
oAuth2CustomParameter_description = Lens.lens (\OAuth2CustomParameter' {description} -> description) (\s@OAuth2CustomParameter' {} a -> s {description = a} :: OAuth2CustomParameter)

-- | Indicates whether the custom parameter for OAuth 2.0 authentication is
-- required.
oAuth2CustomParameter_isRequired :: Lens.Lens' OAuth2CustomParameter (Prelude.Maybe Prelude.Bool)
oAuth2CustomParameter_isRequired = Lens.lens (\OAuth2CustomParameter' {isRequired} -> isRequired) (\s@OAuth2CustomParameter' {} a -> s {isRequired = a} :: OAuth2CustomParameter)

-- | Indicates whether this authentication custom parameter is a sensitive
-- field.
oAuth2CustomParameter_isSensitiveField :: Lens.Lens' OAuth2CustomParameter (Prelude.Maybe Prelude.Bool)
oAuth2CustomParameter_isSensitiveField = Lens.lens (\OAuth2CustomParameter' {isSensitiveField} -> isSensitiveField) (\s@OAuth2CustomParameter' {} a -> s {isSensitiveField = a} :: OAuth2CustomParameter)

-- | The key of the custom parameter required for OAuth 2.0 authentication.
oAuth2CustomParameter_key :: Lens.Lens' OAuth2CustomParameter (Prelude.Maybe Prelude.Text)
oAuth2CustomParameter_key = Lens.lens (\OAuth2CustomParameter' {key} -> key) (\s@OAuth2CustomParameter' {} a -> s {key = a} :: OAuth2CustomParameter)

-- | The label of the custom parameter used for OAuth 2.0 authentication.
oAuth2CustomParameter_label :: Lens.Lens' OAuth2CustomParameter (Prelude.Maybe Prelude.Text)
oAuth2CustomParameter_label = Lens.lens (\OAuth2CustomParameter' {label} -> label) (\s@OAuth2CustomParameter' {} a -> s {label = a} :: OAuth2CustomParameter)

-- | Indicates whether custom parameter is used with TokenUrl or AuthUrl.
oAuth2CustomParameter_type :: Lens.Lens' OAuth2CustomParameter (Prelude.Maybe OAuth2CustomPropType)
oAuth2CustomParameter_type = Lens.lens (\OAuth2CustomParameter' {type'} -> type') (\s@OAuth2CustomParameter' {} a -> s {type' = a} :: OAuth2CustomParameter)

instance Data.FromJSON OAuth2CustomParameter where
  parseJSON =
    Data.withObject
      "OAuth2CustomParameter"
      ( \x ->
          OAuth2CustomParameter'
            Prelude.<$> ( x
                            Data..:? "connectorSuppliedValues"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "isRequired")
            Prelude.<*> (x Data..:? "isSensitiveField")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "label")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable OAuth2CustomParameter where
  hashWithSalt _salt OAuth2CustomParameter' {..} =
    _salt
      `Prelude.hashWithSalt` connectorSuppliedValues
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isRequired
      `Prelude.hashWithSalt` isSensitiveField
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` type'

instance Prelude.NFData OAuth2CustomParameter where
  rnf OAuth2CustomParameter' {..} =
    Prelude.rnf connectorSuppliedValues `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf isRequired `Prelude.seq`
          Prelude.rnf isSensitiveField `Prelude.seq`
            Prelude.rnf key `Prelude.seq`
              Prelude.rnf label `Prelude.seq`
                Prelude.rnf type'
