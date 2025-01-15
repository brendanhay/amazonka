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
-- Module      : Amazonka.SSMSAP.Types.Component
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.Component where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.ComponentStatus
import Amazonka.SSMSAP.Types.ComponentType
import Amazonka.SSMSAP.Types.Host

-- |
--
-- /See:/ 'newComponent' smart constructor.
data Component = Component'
  { applicationId :: Prelude.Maybe Prelude.Text,
    componentId :: Prelude.Maybe Prelude.Text,
    componentType :: Prelude.Maybe ComponentType,
    databases :: Prelude.Maybe [Prelude.Text],
    hosts :: Prelude.Maybe [Host],
    lastUpdated :: Prelude.Maybe Data.POSIX,
    primaryHost :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ComponentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Component' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'component_applicationId' -
--
-- 'componentId', 'component_componentId' -
--
-- 'componentType', 'component_componentType' -
--
-- 'databases', 'component_databases' -
--
-- 'hosts', 'component_hosts' -
--
-- 'lastUpdated', 'component_lastUpdated' -
--
-- 'primaryHost', 'component_primaryHost' -
--
-- 'status', 'component_status' -
newComponent ::
  Component
newComponent =
  Component'
    { applicationId = Prelude.Nothing,
      componentId = Prelude.Nothing,
      componentType = Prelude.Nothing,
      databases = Prelude.Nothing,
      hosts = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      primaryHost = Prelude.Nothing,
      status = Prelude.Nothing
    }

component_applicationId :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_applicationId = Lens.lens (\Component' {applicationId} -> applicationId) (\s@Component' {} a -> s {applicationId = a} :: Component)

component_componentId :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_componentId = Lens.lens (\Component' {componentId} -> componentId) (\s@Component' {} a -> s {componentId = a} :: Component)

component_componentType :: Lens.Lens' Component (Prelude.Maybe ComponentType)
component_componentType = Lens.lens (\Component' {componentType} -> componentType) (\s@Component' {} a -> s {componentType = a} :: Component)

component_databases :: Lens.Lens' Component (Prelude.Maybe [Prelude.Text])
component_databases = Lens.lens (\Component' {databases} -> databases) (\s@Component' {} a -> s {databases = a} :: Component) Prelude.. Lens.mapping Lens.coerced

component_hosts :: Lens.Lens' Component (Prelude.Maybe [Host])
component_hosts = Lens.lens (\Component' {hosts} -> hosts) (\s@Component' {} a -> s {hosts = a} :: Component) Prelude.. Lens.mapping Lens.coerced

component_lastUpdated :: Lens.Lens' Component (Prelude.Maybe Prelude.UTCTime)
component_lastUpdated = Lens.lens (\Component' {lastUpdated} -> lastUpdated) (\s@Component' {} a -> s {lastUpdated = a} :: Component) Prelude.. Lens.mapping Data._Time

component_primaryHost :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_primaryHost = Lens.lens (\Component' {primaryHost} -> primaryHost) (\s@Component' {} a -> s {primaryHost = a} :: Component)

component_status :: Lens.Lens' Component (Prelude.Maybe ComponentStatus)
component_status = Lens.lens (\Component' {status} -> status) (\s@Component' {} a -> s {status = a} :: Component)

instance Data.FromJSON Component where
  parseJSON =
    Data.withObject
      "Component"
      ( \x ->
          Component'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "ComponentId")
            Prelude.<*> (x Data..:? "ComponentType")
            Prelude.<*> (x Data..:? "Databases" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Hosts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "PrimaryHost")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Component where
  hashWithSalt _salt Component' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` databases
      `Prelude.hashWithSalt` hosts
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` primaryHost
      `Prelude.hashWithSalt` status

instance Prelude.NFData Component where
  rnf Component' {..} =
    Prelude.rnf applicationId `Prelude.seq`
      Prelude.rnf componentId `Prelude.seq`
        Prelude.rnf componentType `Prelude.seq`
          Prelude.rnf databases `Prelude.seq`
            Prelude.rnf hosts `Prelude.seq`
              Prelude.rnf lastUpdated `Prelude.seq`
                Prelude.rnf primaryHost `Prelude.seq`
                  Prelude.rnf status
