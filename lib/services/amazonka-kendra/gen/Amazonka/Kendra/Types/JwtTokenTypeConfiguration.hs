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
-- Module      : Amazonka.Kendra.Types.JwtTokenTypeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.JwtTokenTypeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.KeyLocation
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for the JWT token type.
--
-- /See:/ 'newJwtTokenTypeConfiguration' smart constructor.
data JwtTokenTypeConfiguration = JwtTokenTypeConfiguration'
  { -- | The regular expression that identifies the claim.
    claimRegex :: Prelude.Maybe Prelude.Text,
    -- | The group attribute field.
    groupAttributeField :: Prelude.Maybe Prelude.Text,
    -- | The issuer of the token.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (arn) of the secret.
    secretManagerArn :: Prelude.Maybe Prelude.Text,
    -- | The signing key URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The user name attribute field.
    userNameAttributeField :: Prelude.Maybe Prelude.Text,
    -- | The location of the key.
    keyLocation :: KeyLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JwtTokenTypeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'claimRegex', 'jwtTokenTypeConfiguration_claimRegex' - The regular expression that identifies the claim.
--
-- 'groupAttributeField', 'jwtTokenTypeConfiguration_groupAttributeField' - The group attribute field.
--
-- 'issuer', 'jwtTokenTypeConfiguration_issuer' - The issuer of the token.
--
-- 'secretManagerArn', 'jwtTokenTypeConfiguration_secretManagerArn' - The Amazon Resource Name (arn) of the secret.
--
-- 'url', 'jwtTokenTypeConfiguration_url' - The signing key URL.
--
-- 'userNameAttributeField', 'jwtTokenTypeConfiguration_userNameAttributeField' - The user name attribute field.
--
-- 'keyLocation', 'jwtTokenTypeConfiguration_keyLocation' - The location of the key.
newJwtTokenTypeConfiguration ::
  -- | 'keyLocation'
  KeyLocation ->
  JwtTokenTypeConfiguration
newJwtTokenTypeConfiguration pKeyLocation_ =
  JwtTokenTypeConfiguration'
    { claimRegex =
        Prelude.Nothing,
      groupAttributeField = Prelude.Nothing,
      issuer = Prelude.Nothing,
      secretManagerArn = Prelude.Nothing,
      url = Prelude.Nothing,
      userNameAttributeField = Prelude.Nothing,
      keyLocation = pKeyLocation_
    }

-- | The regular expression that identifies the claim.
jwtTokenTypeConfiguration_claimRegex :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_claimRegex = Lens.lens (\JwtTokenTypeConfiguration' {claimRegex} -> claimRegex) (\s@JwtTokenTypeConfiguration' {} a -> s {claimRegex = a} :: JwtTokenTypeConfiguration)

-- | The group attribute field.
jwtTokenTypeConfiguration_groupAttributeField :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_groupAttributeField = Lens.lens (\JwtTokenTypeConfiguration' {groupAttributeField} -> groupAttributeField) (\s@JwtTokenTypeConfiguration' {} a -> s {groupAttributeField = a} :: JwtTokenTypeConfiguration)

-- | The issuer of the token.
jwtTokenTypeConfiguration_issuer :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_issuer = Lens.lens (\JwtTokenTypeConfiguration' {issuer} -> issuer) (\s@JwtTokenTypeConfiguration' {} a -> s {issuer = a} :: JwtTokenTypeConfiguration)

-- | The Amazon Resource Name (arn) of the secret.
jwtTokenTypeConfiguration_secretManagerArn :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_secretManagerArn = Lens.lens (\JwtTokenTypeConfiguration' {secretManagerArn} -> secretManagerArn) (\s@JwtTokenTypeConfiguration' {} a -> s {secretManagerArn = a} :: JwtTokenTypeConfiguration)

-- | The signing key URL.
jwtTokenTypeConfiguration_url :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_url = Lens.lens (\JwtTokenTypeConfiguration' {url} -> url) (\s@JwtTokenTypeConfiguration' {} a -> s {url = a} :: JwtTokenTypeConfiguration)

-- | The user name attribute field.
jwtTokenTypeConfiguration_userNameAttributeField :: Lens.Lens' JwtTokenTypeConfiguration (Prelude.Maybe Prelude.Text)
jwtTokenTypeConfiguration_userNameAttributeField = Lens.lens (\JwtTokenTypeConfiguration' {userNameAttributeField} -> userNameAttributeField) (\s@JwtTokenTypeConfiguration' {} a -> s {userNameAttributeField = a} :: JwtTokenTypeConfiguration)

-- | The location of the key.
jwtTokenTypeConfiguration_keyLocation :: Lens.Lens' JwtTokenTypeConfiguration KeyLocation
jwtTokenTypeConfiguration_keyLocation = Lens.lens (\JwtTokenTypeConfiguration' {keyLocation} -> keyLocation) (\s@JwtTokenTypeConfiguration' {} a -> s {keyLocation = a} :: JwtTokenTypeConfiguration)

instance Data.FromJSON JwtTokenTypeConfiguration where
  parseJSON =
    Data.withObject
      "JwtTokenTypeConfiguration"
      ( \x ->
          JwtTokenTypeConfiguration'
            Prelude.<$> (x Data..:? "ClaimRegex")
            Prelude.<*> (x Data..:? "GroupAttributeField")
            Prelude.<*> (x Data..:? "Issuer")
            Prelude.<*> (x Data..:? "SecretManagerArn")
            Prelude.<*> (x Data..:? "URL")
            Prelude.<*> (x Data..:? "UserNameAttributeField")
            Prelude.<*> (x Data..: "KeyLocation")
      )

instance Prelude.Hashable JwtTokenTypeConfiguration where
  hashWithSalt _salt JwtTokenTypeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` claimRegex
      `Prelude.hashWithSalt` groupAttributeField
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` secretManagerArn
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` userNameAttributeField
      `Prelude.hashWithSalt` keyLocation

instance Prelude.NFData JwtTokenTypeConfiguration where
  rnf JwtTokenTypeConfiguration' {..} =
    Prelude.rnf claimRegex `Prelude.seq`
      Prelude.rnf groupAttributeField `Prelude.seq`
        Prelude.rnf issuer `Prelude.seq`
          Prelude.rnf secretManagerArn `Prelude.seq`
            Prelude.rnf url `Prelude.seq`
              Prelude.rnf userNameAttributeField `Prelude.seq`
                Prelude.rnf keyLocation

instance Data.ToJSON JwtTokenTypeConfiguration where
  toJSON JwtTokenTypeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClaimRegex" Data..=) Prelude.<$> claimRegex,
            ("GroupAttributeField" Data..=)
              Prelude.<$> groupAttributeField,
            ("Issuer" Data..=) Prelude.<$> issuer,
            ("SecretManagerArn" Data..=)
              Prelude.<$> secretManagerArn,
            ("URL" Data..=) Prelude.<$> url,
            ("UserNameAttributeField" Data..=)
              Prelude.<$> userNameAttributeField,
            Prelude.Just ("KeyLocation" Data..= keyLocation)
          ]
      )
