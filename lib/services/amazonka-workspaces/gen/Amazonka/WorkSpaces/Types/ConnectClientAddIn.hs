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
-- Module      : Amazonka.WorkSpaces.Types.ConnectClientAddIn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ConnectClientAddIn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Connect client add-in.
--
-- /See:/ 'newConnectClientAddIn' smart constructor.
data ConnectClientAddIn = ConnectClientAddIn'
  { -- | The directory identifier for which the client add-in is configured.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the client add in.
    name :: Prelude.Maybe Prelude.Text,
    -- | The client add-in identifier.
    addInId :: Prelude.Maybe Prelude.Text,
    -- | The endpoint URL of the client add-in.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectClientAddIn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'connectClientAddIn_resourceId' - The directory identifier for which the client add-in is configured.
--
-- 'name', 'connectClientAddIn_name' - The name of the client add in.
--
-- 'addInId', 'connectClientAddIn_addInId' - The client add-in identifier.
--
-- 'url', 'connectClientAddIn_url' - The endpoint URL of the client add-in.
newConnectClientAddIn ::
  ConnectClientAddIn
newConnectClientAddIn =
  ConnectClientAddIn'
    { resourceId = Prelude.Nothing,
      name = Prelude.Nothing,
      addInId = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The directory identifier for which the client add-in is configured.
connectClientAddIn_resourceId :: Lens.Lens' ConnectClientAddIn (Prelude.Maybe Prelude.Text)
connectClientAddIn_resourceId = Lens.lens (\ConnectClientAddIn' {resourceId} -> resourceId) (\s@ConnectClientAddIn' {} a -> s {resourceId = a} :: ConnectClientAddIn)

-- | The name of the client add in.
connectClientAddIn_name :: Lens.Lens' ConnectClientAddIn (Prelude.Maybe Prelude.Text)
connectClientAddIn_name = Lens.lens (\ConnectClientAddIn' {name} -> name) (\s@ConnectClientAddIn' {} a -> s {name = a} :: ConnectClientAddIn)

-- | The client add-in identifier.
connectClientAddIn_addInId :: Lens.Lens' ConnectClientAddIn (Prelude.Maybe Prelude.Text)
connectClientAddIn_addInId = Lens.lens (\ConnectClientAddIn' {addInId} -> addInId) (\s@ConnectClientAddIn' {} a -> s {addInId = a} :: ConnectClientAddIn)

-- | The endpoint URL of the client add-in.
connectClientAddIn_url :: Lens.Lens' ConnectClientAddIn (Prelude.Maybe Prelude.Text)
connectClientAddIn_url = Lens.lens (\ConnectClientAddIn' {url} -> url) (\s@ConnectClientAddIn' {} a -> s {url = a} :: ConnectClientAddIn)

instance Data.FromJSON ConnectClientAddIn where
  parseJSON =
    Data.withObject
      "ConnectClientAddIn"
      ( \x ->
          ConnectClientAddIn'
            Prelude.<$> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "AddInId")
            Prelude.<*> (x Data..:? "URL")
      )

instance Prelude.Hashable ConnectClientAddIn where
  hashWithSalt _salt ConnectClientAddIn' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` addInId
      `Prelude.hashWithSalt` url

instance Prelude.NFData ConnectClientAddIn where
  rnf ConnectClientAddIn' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf addInId
      `Prelude.seq` Prelude.rnf url
