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
-- Module      : Amazonka.WorkSpaces.Types.ClientPropertiesResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ClientPropertiesResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.ClientProperties

-- | Information about the Amazon WorkSpaces client.
--
-- /See:/ 'newClientPropertiesResult' smart constructor.
data ClientPropertiesResult = ClientPropertiesResult'
  { -- | The resource identifier, in the form of a directory ID.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | Information about the Amazon WorkSpaces client.
    clientProperties :: Prelude.Maybe ClientProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientPropertiesResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'clientPropertiesResult_resourceId' - The resource identifier, in the form of a directory ID.
--
-- 'clientProperties', 'clientPropertiesResult_clientProperties' - Information about the Amazon WorkSpaces client.
newClientPropertiesResult ::
  ClientPropertiesResult
newClientPropertiesResult =
  ClientPropertiesResult'
    { resourceId =
        Prelude.Nothing,
      clientProperties = Prelude.Nothing
    }

-- | The resource identifier, in the form of a directory ID.
clientPropertiesResult_resourceId :: Lens.Lens' ClientPropertiesResult (Prelude.Maybe Prelude.Text)
clientPropertiesResult_resourceId = Lens.lens (\ClientPropertiesResult' {resourceId} -> resourceId) (\s@ClientPropertiesResult' {} a -> s {resourceId = a} :: ClientPropertiesResult)

-- | Information about the Amazon WorkSpaces client.
clientPropertiesResult_clientProperties :: Lens.Lens' ClientPropertiesResult (Prelude.Maybe ClientProperties)
clientPropertiesResult_clientProperties = Lens.lens (\ClientPropertiesResult' {clientProperties} -> clientProperties) (\s@ClientPropertiesResult' {} a -> s {clientProperties = a} :: ClientPropertiesResult)

instance Data.FromJSON ClientPropertiesResult where
  parseJSON =
    Data.withObject
      "ClientPropertiesResult"
      ( \x ->
          ClientPropertiesResult'
            Prelude.<$> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "ClientProperties")
      )

instance Prelude.Hashable ClientPropertiesResult where
  hashWithSalt _salt ClientPropertiesResult' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` clientProperties

instance Prelude.NFData ClientPropertiesResult where
  rnf ClientPropertiesResult' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf clientProperties
