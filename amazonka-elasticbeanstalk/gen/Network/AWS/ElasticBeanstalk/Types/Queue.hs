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
-- Module      : Network.AWS.ElasticBeanstalk.Types.Queue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Queue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a queue.
--
-- /See:/ 'newQueue' smart constructor.
data Queue = Queue'
  { -- | The name of the queue.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URL of the queue.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Queue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'queue_name' - The name of the queue.
--
-- 'url', 'queue_url' - The URL of the queue.
newQueue ::
  Queue
newQueue =
  Queue'
    { name = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The name of the queue.
queue_name :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_name = Lens.lens (\Queue' {name} -> name) (\s@Queue' {} a -> s {name = a} :: Queue)

-- | The URL of the queue.
queue_url :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_url = Lens.lens (\Queue' {url} -> url) (\s@Queue' {} a -> s {url = a} :: Queue)

instance Core.FromXML Queue where
  parseXML x =
    Queue'
      Prelude.<$> (x Core..@? "Name") Prelude.<*> (x Core..@? "URL")

instance Prelude.Hashable Queue

instance Prelude.NFData Queue
