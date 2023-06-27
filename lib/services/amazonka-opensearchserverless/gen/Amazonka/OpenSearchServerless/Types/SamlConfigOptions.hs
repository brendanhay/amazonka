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
-- Module      : Amazonka.OpenSearchServerless.Types.SamlConfigOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.SamlConfigOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes SAML options for an OpenSearch Serverless security
-- configuration in the form of a key-value map.
--
-- /See:/ 'newSamlConfigOptions' smart constructor.
data SamlConfigOptions = SamlConfigOptions'
  { -- | The group attribute for this SAML integration.
    groupAttribute :: Prelude.Maybe Prelude.Text,
    -- | The session timeout, in minutes. Default is 60 minutes (12 hours).
    sessionTimeout :: Prelude.Maybe Prelude.Natural,
    -- | A user attribute for this SAML integration.
    userAttribute :: Prelude.Maybe Prelude.Text,
    -- | The XML IdP metadata file generated from your identity provider.
    metadata :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SamlConfigOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupAttribute', 'samlConfigOptions_groupAttribute' - The group attribute for this SAML integration.
--
-- 'sessionTimeout', 'samlConfigOptions_sessionTimeout' - The session timeout, in minutes. Default is 60 minutes (12 hours).
--
-- 'userAttribute', 'samlConfigOptions_userAttribute' - A user attribute for this SAML integration.
--
-- 'metadata', 'samlConfigOptions_metadata' - The XML IdP metadata file generated from your identity provider.
newSamlConfigOptions ::
  -- | 'metadata'
  Prelude.Text ->
  SamlConfigOptions
newSamlConfigOptions pMetadata_ =
  SamlConfigOptions'
    { groupAttribute =
        Prelude.Nothing,
      sessionTimeout = Prelude.Nothing,
      userAttribute = Prelude.Nothing,
      metadata = pMetadata_
    }

-- | The group attribute for this SAML integration.
samlConfigOptions_groupAttribute :: Lens.Lens' SamlConfigOptions (Prelude.Maybe Prelude.Text)
samlConfigOptions_groupAttribute = Lens.lens (\SamlConfigOptions' {groupAttribute} -> groupAttribute) (\s@SamlConfigOptions' {} a -> s {groupAttribute = a} :: SamlConfigOptions)

-- | The session timeout, in minutes. Default is 60 minutes (12 hours).
samlConfigOptions_sessionTimeout :: Lens.Lens' SamlConfigOptions (Prelude.Maybe Prelude.Natural)
samlConfigOptions_sessionTimeout = Lens.lens (\SamlConfigOptions' {sessionTimeout} -> sessionTimeout) (\s@SamlConfigOptions' {} a -> s {sessionTimeout = a} :: SamlConfigOptions)

-- | A user attribute for this SAML integration.
samlConfigOptions_userAttribute :: Lens.Lens' SamlConfigOptions (Prelude.Maybe Prelude.Text)
samlConfigOptions_userAttribute = Lens.lens (\SamlConfigOptions' {userAttribute} -> userAttribute) (\s@SamlConfigOptions' {} a -> s {userAttribute = a} :: SamlConfigOptions)

-- | The XML IdP metadata file generated from your identity provider.
samlConfigOptions_metadata :: Lens.Lens' SamlConfigOptions Prelude.Text
samlConfigOptions_metadata = Lens.lens (\SamlConfigOptions' {metadata} -> metadata) (\s@SamlConfigOptions' {} a -> s {metadata = a} :: SamlConfigOptions)

instance Data.FromJSON SamlConfigOptions where
  parseJSON =
    Data.withObject
      "SamlConfigOptions"
      ( \x ->
          SamlConfigOptions'
            Prelude.<$> (x Data..:? "groupAttribute")
            Prelude.<*> (x Data..:? "sessionTimeout")
            Prelude.<*> (x Data..:? "userAttribute")
            Prelude.<*> (x Data..: "metadata")
      )

instance Prelude.Hashable SamlConfigOptions where
  hashWithSalt _salt SamlConfigOptions' {..} =
    _salt
      `Prelude.hashWithSalt` groupAttribute
      `Prelude.hashWithSalt` sessionTimeout
      `Prelude.hashWithSalt` userAttribute
      `Prelude.hashWithSalt` metadata

instance Prelude.NFData SamlConfigOptions where
  rnf SamlConfigOptions' {..} =
    Prelude.rnf groupAttribute
      `Prelude.seq` Prelude.rnf sessionTimeout
      `Prelude.seq` Prelude.rnf userAttribute
      `Prelude.seq` Prelude.rnf metadata

instance Data.ToJSON SamlConfigOptions where
  toJSON SamlConfigOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("groupAttribute" Data..=)
              Prelude.<$> groupAttribute,
            ("sessionTimeout" Data..=)
              Prelude.<$> sessionTimeout,
            ("userAttribute" Data..=) Prelude.<$> userAttribute,
            Prelude.Just ("metadata" Data..= metadata)
          ]
      )
