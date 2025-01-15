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
-- Module      : Amazonka.LicenseManager.Types.TokenData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.TokenData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a token.
--
-- /See:/ 'newTokenData' smart constructor.
data TokenData = TokenData'
  { -- | Token expiration time, in ISO8601-UTC format.
    expirationTime :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the license.
    licenseArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Names (ARN) of the roles included in the token.
    roleArns :: Prelude.Maybe [Prelude.Text],
    -- | Token status. The possible values are @AVAILABLE@ and @DELETED@.
    status :: Prelude.Maybe Prelude.Text,
    -- | Token ID.
    tokenId :: Prelude.Maybe Prelude.Text,
    -- | Data specified by the caller.
    tokenProperties :: Prelude.Maybe [Prelude.Text],
    -- | Type of token generated. The supported value is @REFRESH_TOKEN@.
    tokenType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TokenData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationTime', 'tokenData_expirationTime' - Token expiration time, in ISO8601-UTC format.
--
-- 'licenseArn', 'tokenData_licenseArn' - Amazon Resource Name (ARN) of the license.
--
-- 'roleArns', 'tokenData_roleArns' - Amazon Resource Names (ARN) of the roles included in the token.
--
-- 'status', 'tokenData_status' - Token status. The possible values are @AVAILABLE@ and @DELETED@.
--
-- 'tokenId', 'tokenData_tokenId' - Token ID.
--
-- 'tokenProperties', 'tokenData_tokenProperties' - Data specified by the caller.
--
-- 'tokenType', 'tokenData_tokenType' - Type of token generated. The supported value is @REFRESH_TOKEN@.
newTokenData ::
  TokenData
newTokenData =
  TokenData'
    { expirationTime = Prelude.Nothing,
      licenseArn = Prelude.Nothing,
      roleArns = Prelude.Nothing,
      status = Prelude.Nothing,
      tokenId = Prelude.Nothing,
      tokenProperties = Prelude.Nothing,
      tokenType = Prelude.Nothing
    }

-- | Token expiration time, in ISO8601-UTC format.
tokenData_expirationTime :: Lens.Lens' TokenData (Prelude.Maybe Prelude.Text)
tokenData_expirationTime = Lens.lens (\TokenData' {expirationTime} -> expirationTime) (\s@TokenData' {} a -> s {expirationTime = a} :: TokenData)

-- | Amazon Resource Name (ARN) of the license.
tokenData_licenseArn :: Lens.Lens' TokenData (Prelude.Maybe Prelude.Text)
tokenData_licenseArn = Lens.lens (\TokenData' {licenseArn} -> licenseArn) (\s@TokenData' {} a -> s {licenseArn = a} :: TokenData)

-- | Amazon Resource Names (ARN) of the roles included in the token.
tokenData_roleArns :: Lens.Lens' TokenData (Prelude.Maybe [Prelude.Text])
tokenData_roleArns = Lens.lens (\TokenData' {roleArns} -> roleArns) (\s@TokenData' {} a -> s {roleArns = a} :: TokenData) Prelude.. Lens.mapping Lens.coerced

-- | Token status. The possible values are @AVAILABLE@ and @DELETED@.
tokenData_status :: Lens.Lens' TokenData (Prelude.Maybe Prelude.Text)
tokenData_status = Lens.lens (\TokenData' {status} -> status) (\s@TokenData' {} a -> s {status = a} :: TokenData)

-- | Token ID.
tokenData_tokenId :: Lens.Lens' TokenData (Prelude.Maybe Prelude.Text)
tokenData_tokenId = Lens.lens (\TokenData' {tokenId} -> tokenId) (\s@TokenData' {} a -> s {tokenId = a} :: TokenData)

-- | Data specified by the caller.
tokenData_tokenProperties :: Lens.Lens' TokenData (Prelude.Maybe [Prelude.Text])
tokenData_tokenProperties = Lens.lens (\TokenData' {tokenProperties} -> tokenProperties) (\s@TokenData' {} a -> s {tokenProperties = a} :: TokenData) Prelude.. Lens.mapping Lens.coerced

-- | Type of token generated. The supported value is @REFRESH_TOKEN@.
tokenData_tokenType :: Lens.Lens' TokenData (Prelude.Maybe Prelude.Text)
tokenData_tokenType = Lens.lens (\TokenData' {tokenType} -> tokenType) (\s@TokenData' {} a -> s {tokenType = a} :: TokenData)

instance Data.FromJSON TokenData where
  parseJSON =
    Data.withObject
      "TokenData"
      ( \x ->
          TokenData'
            Prelude.<$> (x Data..:? "ExpirationTime")
            Prelude.<*> (x Data..:? "LicenseArn")
            Prelude.<*> (x Data..:? "RoleArns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TokenId")
            Prelude.<*> ( x
                            Data..:? "TokenProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "TokenType")
      )

instance Prelude.Hashable TokenData where
  hashWithSalt _salt TokenData' {..} =
    _salt
      `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` licenseArn
      `Prelude.hashWithSalt` roleArns
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tokenId
      `Prelude.hashWithSalt` tokenProperties
      `Prelude.hashWithSalt` tokenType

instance Prelude.NFData TokenData where
  rnf TokenData' {..} =
    Prelude.rnf expirationTime `Prelude.seq`
      Prelude.rnf licenseArn `Prelude.seq`
        Prelude.rnf roleArns `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf tokenId `Prelude.seq`
              Prelude.rnf tokenProperties `Prelude.seq`
                Prelude.rnf tokenType
