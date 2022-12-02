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
-- Module      : Amazonka.ConnectCases.Types.DomainSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.DomainSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object for the summarized details of the domain.
--
-- /See:/ 'newDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { -- | The Amazon Resource Name (ARN) of the domain.
    domainArn :: Prelude.Text,
    -- | The unique identifier of the domain.
    domainId :: Prelude.Text,
    -- | The name of the domain.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainArn', 'domainSummary_domainArn' - The Amazon Resource Name (ARN) of the domain.
--
-- 'domainId', 'domainSummary_domainId' - The unique identifier of the domain.
--
-- 'name', 'domainSummary_name' - The name of the domain.
newDomainSummary ::
  -- | 'domainArn'
  Prelude.Text ->
  -- | 'domainId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  DomainSummary
newDomainSummary pDomainArn_ pDomainId_ pName_ =
  DomainSummary'
    { domainArn = pDomainArn_,
      domainId = pDomainId_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) of the domain.
domainSummary_domainArn :: Lens.Lens' DomainSummary Prelude.Text
domainSummary_domainArn = Lens.lens (\DomainSummary' {domainArn} -> domainArn) (\s@DomainSummary' {} a -> s {domainArn = a} :: DomainSummary)

-- | The unique identifier of the domain.
domainSummary_domainId :: Lens.Lens' DomainSummary Prelude.Text
domainSummary_domainId = Lens.lens (\DomainSummary' {domainId} -> domainId) (\s@DomainSummary' {} a -> s {domainId = a} :: DomainSummary)

-- | The name of the domain.
domainSummary_name :: Lens.Lens' DomainSummary Prelude.Text
domainSummary_name = Lens.lens (\DomainSummary' {name} -> name) (\s@DomainSummary' {} a -> s {name = a} :: DomainSummary)

instance Data.FromJSON DomainSummary where
  parseJSON =
    Data.withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            Prelude.<$> (x Data..: "domainArn")
            Prelude.<*> (x Data..: "domainId")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable DomainSummary where
  hashWithSalt _salt DomainSummary' {..} =
    _salt `Prelude.hashWithSalt` domainArn
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` name

instance Prelude.NFData DomainSummary where
  rnf DomainSummary' {..} =
    Prelude.rnf domainArn
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf name
