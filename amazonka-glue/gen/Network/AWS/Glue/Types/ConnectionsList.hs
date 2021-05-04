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
-- Module      : Network.AWS.Glue.Types.ConnectionsList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionsList where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the connections used by a job.
--
-- /See:/ 'newConnectionsList' smart constructor.
data ConnectionsList = ConnectionsList'
  { -- | A list of connections used by the job.
    connections :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConnectionsList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connections', 'connectionsList_connections' - A list of connections used by the job.
newConnectionsList ::
  ConnectionsList
newConnectionsList =
  ConnectionsList' {connections = Prelude.Nothing}

-- | A list of connections used by the job.
connectionsList_connections :: Lens.Lens' ConnectionsList (Prelude.Maybe [Prelude.Text])
connectionsList_connections = Lens.lens (\ConnectionsList' {connections} -> connections) (\s@ConnectionsList' {} a -> s {connections = a} :: ConnectionsList) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ConnectionsList where
  parseJSON =
    Prelude.withObject
      "ConnectionsList"
      ( \x ->
          ConnectionsList'
            Prelude.<$> ( x Prelude..:? "Connections"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ConnectionsList

instance Prelude.NFData ConnectionsList

instance Prelude.ToJSON ConnectionsList where
  toJSON ConnectionsList' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Connections" Prelude..=) Prelude.<$> connections]
      )
