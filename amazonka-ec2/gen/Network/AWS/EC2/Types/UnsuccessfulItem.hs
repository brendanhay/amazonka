{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.UnsuccessfulItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UnsuccessfulItemError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about items that were not successfully processed in a batch
-- call.
--
-- /See:/ 'newUnsuccessfulItem' smart constructor.
data UnsuccessfulItem = UnsuccessfulItem'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | Information about the error.
    error :: Prelude.Maybe UnsuccessfulItemError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnsuccessfulItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'unsuccessfulItem_resourceId' - The ID of the resource.
--
-- 'error', 'unsuccessfulItem_error' - Information about the error.
newUnsuccessfulItem ::
  UnsuccessfulItem
newUnsuccessfulItem =
  UnsuccessfulItem'
    { resourceId = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The ID of the resource.
unsuccessfulItem_resourceId :: Lens.Lens' UnsuccessfulItem (Prelude.Maybe Prelude.Text)
unsuccessfulItem_resourceId = Lens.lens (\UnsuccessfulItem' {resourceId} -> resourceId) (\s@UnsuccessfulItem' {} a -> s {resourceId = a} :: UnsuccessfulItem)

-- | Information about the error.
unsuccessfulItem_error :: Lens.Lens' UnsuccessfulItem (Prelude.Maybe UnsuccessfulItemError)
unsuccessfulItem_error = Lens.lens (\UnsuccessfulItem' {error} -> error) (\s@UnsuccessfulItem' {} a -> s {error = a} :: UnsuccessfulItem)

instance Prelude.FromXML UnsuccessfulItem where
  parseXML x =
    UnsuccessfulItem'
      Prelude.<$> (x Prelude..@? "resourceId")
      Prelude.<*> (x Prelude..@? "error")

instance Prelude.Hashable UnsuccessfulItem

instance Prelude.NFData UnsuccessfulItem
