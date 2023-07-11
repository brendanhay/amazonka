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
-- Module      : Amazonka.SWF.Types.DomainInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.DomainInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.RegistrationStatus

-- | Contains general information about a domain.
--
-- /See:/ 'newDomainInfo' smart constructor.
data DomainInfo = DomainInfo'
  { -- | The ARN of the domain.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the domain provided through RegisterDomain.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain. This name is unique within the account.
    name :: Prelude.Text,
    -- | The status of the domain:
    --
    -- -   @REGISTERED@ – The domain is properly registered and available. You
    --     can use this domain for registering types and creating new workflow
    --     executions.
    --
    -- -   @DEPRECATED@ – The domain was deprecated using DeprecateDomain, but
    --     is still in use. You should not create new workflow executions in
    --     this domain.
    status :: RegistrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'domainInfo_arn' - The ARN of the domain.
--
-- 'description', 'domainInfo_description' - The description of the domain provided through RegisterDomain.
--
-- 'name', 'domainInfo_name' - The name of the domain. This name is unique within the account.
--
-- 'status', 'domainInfo_status' - The status of the domain:
--
-- -   @REGISTERED@ – The domain is properly registered and available. You
--     can use this domain for registering types and creating new workflow
--     executions.
--
-- -   @DEPRECATED@ – The domain was deprecated using DeprecateDomain, but
--     is still in use. You should not create new workflow executions in
--     this domain.
newDomainInfo ::
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  RegistrationStatus ->
  DomainInfo
newDomainInfo pName_ pStatus_ =
  DomainInfo'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      status = pStatus_
    }

-- | The ARN of the domain.
domainInfo_arn :: Lens.Lens' DomainInfo (Prelude.Maybe Prelude.Text)
domainInfo_arn = Lens.lens (\DomainInfo' {arn} -> arn) (\s@DomainInfo' {} a -> s {arn = a} :: DomainInfo)

-- | The description of the domain provided through RegisterDomain.
domainInfo_description :: Lens.Lens' DomainInfo (Prelude.Maybe Prelude.Text)
domainInfo_description = Lens.lens (\DomainInfo' {description} -> description) (\s@DomainInfo' {} a -> s {description = a} :: DomainInfo)

-- | The name of the domain. This name is unique within the account.
domainInfo_name :: Lens.Lens' DomainInfo Prelude.Text
domainInfo_name = Lens.lens (\DomainInfo' {name} -> name) (\s@DomainInfo' {} a -> s {name = a} :: DomainInfo)

-- | The status of the domain:
--
-- -   @REGISTERED@ – The domain is properly registered and available. You
--     can use this domain for registering types and creating new workflow
--     executions.
--
-- -   @DEPRECATED@ – The domain was deprecated using DeprecateDomain, but
--     is still in use. You should not create new workflow executions in
--     this domain.
domainInfo_status :: Lens.Lens' DomainInfo RegistrationStatus
domainInfo_status = Lens.lens (\DomainInfo' {status} -> status) (\s@DomainInfo' {} a -> s {status = a} :: DomainInfo)

instance Data.FromJSON DomainInfo where
  parseJSON =
    Data.withObject
      "DomainInfo"
      ( \x ->
          DomainInfo'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable DomainInfo where
  hashWithSalt _salt DomainInfo' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData DomainInfo where
  rnf DomainInfo' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
