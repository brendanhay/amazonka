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
-- Module      : Amazonka.CodeArtifact.Types.RepositorySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.RepositorySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a repository, including its Amazon Resource Name (ARN),
-- description, and domain information. The
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_ListRepositories.html ListRepositories>
-- operation returns a list of @RepositorySummary@ objects.
--
-- /See:/ 'newRepositorySummary' smart constructor.
data RepositorySummary = RepositorySummary'
  { -- | The Amazon Web Services account ID that manages the repository.
    administratorAccount :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the repository.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the repository.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositorySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'administratorAccount', 'repositorySummary_administratorAccount' - The Amazon Web Services account ID that manages the repository.
--
-- 'arn', 'repositorySummary_arn' - The ARN of the repository.
--
-- 'description', 'repositorySummary_description' - The description of the repository.
--
-- 'domainName', 'repositorySummary_domainName' - The name of the domain that contains the repository.
--
-- 'domainOwner', 'repositorySummary_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'name', 'repositorySummary_name' - The name of the repository.
newRepositorySummary ::
  RepositorySummary
newRepositorySummary =
  RepositorySummary'
    { administratorAccount =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      domainName = Prelude.Nothing,
      domainOwner = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Web Services account ID that manages the repository.
repositorySummary_administratorAccount :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_administratorAccount = Lens.lens (\RepositorySummary' {administratorAccount} -> administratorAccount) (\s@RepositorySummary' {} a -> s {administratorAccount = a} :: RepositorySummary)

-- | The ARN of the repository.
repositorySummary_arn :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_arn = Lens.lens (\RepositorySummary' {arn} -> arn) (\s@RepositorySummary' {} a -> s {arn = a} :: RepositorySummary)

-- | The description of the repository.
repositorySummary_description :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_description = Lens.lens (\RepositorySummary' {description} -> description) (\s@RepositorySummary' {} a -> s {description = a} :: RepositorySummary)

-- | The name of the domain that contains the repository.
repositorySummary_domainName :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_domainName = Lens.lens (\RepositorySummary' {domainName} -> domainName) (\s@RepositorySummary' {} a -> s {domainName = a} :: RepositorySummary)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
repositorySummary_domainOwner :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_domainOwner = Lens.lens (\RepositorySummary' {domainOwner} -> domainOwner) (\s@RepositorySummary' {} a -> s {domainOwner = a} :: RepositorySummary)

-- | The name of the repository.
repositorySummary_name :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_name = Lens.lens (\RepositorySummary' {name} -> name) (\s@RepositorySummary' {} a -> s {name = a} :: RepositorySummary)

instance Data.FromJSON RepositorySummary where
  parseJSON =
    Data.withObject
      "RepositorySummary"
      ( \x ->
          RepositorySummary'
            Prelude.<$> (x Data..:? "administratorAccount")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "domainName")
            Prelude.<*> (x Data..:? "domainOwner")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable RepositorySummary where
  hashWithSalt _salt RepositorySummary' {..} =
    _salt
      `Prelude.hashWithSalt` administratorAccount
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` name

instance Prelude.NFData RepositorySummary where
  rnf RepositorySummary' {..} =
    Prelude.rnf administratorAccount
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf name
