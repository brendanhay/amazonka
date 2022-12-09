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
-- Module      : Amazonka.EC2.Types.UnsuccessfulItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.UnsuccessfulItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.UnsuccessfulItemError
import qualified Amazonka.Prelude as Prelude

-- | Information about items that were not successfully processed in a batch
-- call.
--
-- /See:/ 'newUnsuccessfulItem' smart constructor.
data UnsuccessfulItem = UnsuccessfulItem'
  { -- | Information about the error.
    error :: Prelude.Maybe UnsuccessfulItemError,
    -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnsuccessfulItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'unsuccessfulItem_error' - Information about the error.
--
-- 'resourceId', 'unsuccessfulItem_resourceId' - The ID of the resource.
newUnsuccessfulItem ::
  UnsuccessfulItem
newUnsuccessfulItem =
  UnsuccessfulItem'
    { error = Prelude.Nothing,
      resourceId = Prelude.Nothing
    }

-- | Information about the error.
unsuccessfulItem_error :: Lens.Lens' UnsuccessfulItem (Prelude.Maybe UnsuccessfulItemError)
unsuccessfulItem_error = Lens.lens (\UnsuccessfulItem' {error} -> error) (\s@UnsuccessfulItem' {} a -> s {error = a} :: UnsuccessfulItem)

-- | The ID of the resource.
unsuccessfulItem_resourceId :: Lens.Lens' UnsuccessfulItem (Prelude.Maybe Prelude.Text)
unsuccessfulItem_resourceId = Lens.lens (\UnsuccessfulItem' {resourceId} -> resourceId) (\s@UnsuccessfulItem' {} a -> s {resourceId = a} :: UnsuccessfulItem)

instance Data.FromXML UnsuccessfulItem where
  parseXML x =
    UnsuccessfulItem'
      Prelude.<$> (x Data..@? "error")
      Prelude.<*> (x Data..@? "resourceId")

instance Prelude.Hashable UnsuccessfulItem where
  hashWithSalt _salt UnsuccessfulItem' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData UnsuccessfulItem where
  rnf UnsuccessfulItem' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf resourceId
