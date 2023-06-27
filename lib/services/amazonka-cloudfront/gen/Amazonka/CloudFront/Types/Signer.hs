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
-- Module      : Amazonka.CloudFront.Types.Signer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.Signer where

import Amazonka.CloudFront.Types.KeyPairIds
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of Amazon Web Services accounts and the active CloudFront key
-- pairs in each account that CloudFront can use to verify the signatures
-- of signed URLs and signed cookies.
--
-- /See:/ 'newSigner' smart constructor.
data Signer = Signer'
  { -- | An Amazon Web Services account number that contains active CloudFront
    -- key pairs that CloudFront can use to verify the signatures of signed
    -- URLs and signed cookies. If the Amazon Web Services account that owns
    -- the key pairs is the same account that owns the CloudFront distribution,
    -- the value of this field is @self@.
    awsAccountNumber :: Prelude.Maybe Prelude.Text,
    -- | A list of CloudFront key pair identifiers.
    keyPairIds :: Prelude.Maybe KeyPairIds
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Signer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountNumber', 'signer_awsAccountNumber' - An Amazon Web Services account number that contains active CloudFront
-- key pairs that CloudFront can use to verify the signatures of signed
-- URLs and signed cookies. If the Amazon Web Services account that owns
-- the key pairs is the same account that owns the CloudFront distribution,
-- the value of this field is @self@.
--
-- 'keyPairIds', 'signer_keyPairIds' - A list of CloudFront key pair identifiers.
newSigner ::
  Signer
newSigner =
  Signer'
    { awsAccountNumber = Prelude.Nothing,
      keyPairIds = Prelude.Nothing
    }

-- | An Amazon Web Services account number that contains active CloudFront
-- key pairs that CloudFront can use to verify the signatures of signed
-- URLs and signed cookies. If the Amazon Web Services account that owns
-- the key pairs is the same account that owns the CloudFront distribution,
-- the value of this field is @self@.
signer_awsAccountNumber :: Lens.Lens' Signer (Prelude.Maybe Prelude.Text)
signer_awsAccountNumber = Lens.lens (\Signer' {awsAccountNumber} -> awsAccountNumber) (\s@Signer' {} a -> s {awsAccountNumber = a} :: Signer)

-- | A list of CloudFront key pair identifiers.
signer_keyPairIds :: Lens.Lens' Signer (Prelude.Maybe KeyPairIds)
signer_keyPairIds = Lens.lens (\Signer' {keyPairIds} -> keyPairIds) (\s@Signer' {} a -> s {keyPairIds = a} :: Signer)

instance Data.FromXML Signer where
  parseXML x =
    Signer'
      Prelude.<$> (x Data..@? "AwsAccountNumber")
      Prelude.<*> (x Data..@? "KeyPairIds")

instance Prelude.Hashable Signer where
  hashWithSalt _salt Signer' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountNumber
      `Prelude.hashWithSalt` keyPairIds

instance Prelude.NFData Signer where
  rnf Signer' {..} =
    Prelude.rnf awsAccountNumber
      `Prelude.seq` Prelude.rnf keyPairIds
