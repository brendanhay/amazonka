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
-- Module      : Network.AWS.Connect.Types.QueueReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QueueReference where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a queue resource for which metrics are
-- returned.
--
-- /See:/ 'newQueueReference' smart constructor.
data QueueReference = QueueReference'
  { -- | The Amazon Resource Name (ARN) of the queue.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the queue.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'QueueReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'queueReference_arn' - The Amazon Resource Name (ARN) of the queue.
--
-- 'id', 'queueReference_id' - The identifier of the queue.
newQueueReference ::
  QueueReference
newQueueReference =
  QueueReference'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the queue.
queueReference_arn :: Lens.Lens' QueueReference (Prelude.Maybe Prelude.Text)
queueReference_arn = Lens.lens (\QueueReference' {arn} -> arn) (\s@QueueReference' {} a -> s {arn = a} :: QueueReference)

-- | The identifier of the queue.
queueReference_id :: Lens.Lens' QueueReference (Prelude.Maybe Prelude.Text)
queueReference_id = Lens.lens (\QueueReference' {id} -> id) (\s@QueueReference' {} a -> s {id = a} :: QueueReference)

instance Prelude.FromJSON QueueReference where
  parseJSON =
    Prelude.withObject
      "QueueReference"
      ( \x ->
          QueueReference'
            Prelude.<$> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
      )

instance Prelude.Hashable QueueReference

instance Prelude.NFData QueueReference
