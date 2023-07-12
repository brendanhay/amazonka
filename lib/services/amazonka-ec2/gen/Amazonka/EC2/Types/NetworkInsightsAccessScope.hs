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
-- Module      : Amazonka.EC2.Types.NetworkInsightsAccessScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInsightsAccessScope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a Network Access Scope.
--
-- /See:/ 'newNetworkInsightsAccessScope' smart constructor.
data NetworkInsightsAccessScope = NetworkInsightsAccessScope'
  { -- | The creation date.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the Network Access Scope.
    networkInsightsAccessScopeArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Network Access Scope.
    networkInsightsAccessScopeId :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe [Tag],
    -- | The last updated date.
    updatedDate :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInsightsAccessScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'networkInsightsAccessScope_createdDate' - The creation date.
--
-- 'networkInsightsAccessScopeArn', 'networkInsightsAccessScope_networkInsightsAccessScopeArn' - The Amazon Resource Name (ARN) of the Network Access Scope.
--
-- 'networkInsightsAccessScopeId', 'networkInsightsAccessScope_networkInsightsAccessScopeId' - The ID of the Network Access Scope.
--
-- 'tags', 'networkInsightsAccessScope_tags' - The tags.
--
-- 'updatedDate', 'networkInsightsAccessScope_updatedDate' - The last updated date.
newNetworkInsightsAccessScope ::
  NetworkInsightsAccessScope
newNetworkInsightsAccessScope =
  NetworkInsightsAccessScope'
    { createdDate =
        Prelude.Nothing,
      networkInsightsAccessScopeArn = Prelude.Nothing,
      networkInsightsAccessScopeId = Prelude.Nothing,
      tags = Prelude.Nothing,
      updatedDate = Prelude.Nothing
    }

-- | The creation date.
networkInsightsAccessScope_createdDate :: Lens.Lens' NetworkInsightsAccessScope (Prelude.Maybe Prelude.UTCTime)
networkInsightsAccessScope_createdDate = Lens.lens (\NetworkInsightsAccessScope' {createdDate} -> createdDate) (\s@NetworkInsightsAccessScope' {} a -> s {createdDate = a} :: NetworkInsightsAccessScope) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the Network Access Scope.
networkInsightsAccessScope_networkInsightsAccessScopeArn :: Lens.Lens' NetworkInsightsAccessScope (Prelude.Maybe Prelude.Text)
networkInsightsAccessScope_networkInsightsAccessScopeArn = Lens.lens (\NetworkInsightsAccessScope' {networkInsightsAccessScopeArn} -> networkInsightsAccessScopeArn) (\s@NetworkInsightsAccessScope' {} a -> s {networkInsightsAccessScopeArn = a} :: NetworkInsightsAccessScope)

-- | The ID of the Network Access Scope.
networkInsightsAccessScope_networkInsightsAccessScopeId :: Lens.Lens' NetworkInsightsAccessScope (Prelude.Maybe Prelude.Text)
networkInsightsAccessScope_networkInsightsAccessScopeId = Lens.lens (\NetworkInsightsAccessScope' {networkInsightsAccessScopeId} -> networkInsightsAccessScopeId) (\s@NetworkInsightsAccessScope' {} a -> s {networkInsightsAccessScopeId = a} :: NetworkInsightsAccessScope)

-- | The tags.
networkInsightsAccessScope_tags :: Lens.Lens' NetworkInsightsAccessScope (Prelude.Maybe [Tag])
networkInsightsAccessScope_tags = Lens.lens (\NetworkInsightsAccessScope' {tags} -> tags) (\s@NetworkInsightsAccessScope' {} a -> s {tags = a} :: NetworkInsightsAccessScope) Prelude.. Lens.mapping Lens.coerced

-- | The last updated date.
networkInsightsAccessScope_updatedDate :: Lens.Lens' NetworkInsightsAccessScope (Prelude.Maybe Prelude.UTCTime)
networkInsightsAccessScope_updatedDate = Lens.lens (\NetworkInsightsAccessScope' {updatedDate} -> updatedDate) (\s@NetworkInsightsAccessScope' {} a -> s {updatedDate = a} :: NetworkInsightsAccessScope) Prelude.. Lens.mapping Data._Time

instance Data.FromXML NetworkInsightsAccessScope where
  parseXML x =
    NetworkInsightsAccessScope'
      Prelude.<$> (x Data..@? "createdDate")
      Prelude.<*> (x Data..@? "networkInsightsAccessScopeArn")
      Prelude.<*> (x Data..@? "networkInsightsAccessScopeId")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "updatedDate")

instance Prelude.Hashable NetworkInsightsAccessScope where
  hashWithSalt _salt NetworkInsightsAccessScope' {..} =
    _salt
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` networkInsightsAccessScopeArn
      `Prelude.hashWithSalt` networkInsightsAccessScopeId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` updatedDate

instance Prelude.NFData NetworkInsightsAccessScope where
  rnf NetworkInsightsAccessScope' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeArn
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updatedDate
