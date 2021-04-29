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
-- Module      : Network.AWS.Connect.Types.InstanceStatusReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStatusReason where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Relevant details why the instance was not successfully created.
--
-- /See:/ 'newInstanceStatusReason' smart constructor.
data InstanceStatusReason = InstanceStatusReason'
  { -- | The message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceStatusReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'instanceStatusReason_message' - The message.
newInstanceStatusReason ::
  InstanceStatusReason
newInstanceStatusReason =
  InstanceStatusReason' {message = Prelude.Nothing}

-- | The message.
instanceStatusReason_message :: Lens.Lens' InstanceStatusReason (Prelude.Maybe Prelude.Text)
instanceStatusReason_message = Lens.lens (\InstanceStatusReason' {message} -> message) (\s@InstanceStatusReason' {} a -> s {message = a} :: InstanceStatusReason)

instance Prelude.FromJSON InstanceStatusReason where
  parseJSON =
    Prelude.withObject
      "InstanceStatusReason"
      ( \x ->
          InstanceStatusReason'
            Prelude.<$> (x Prelude..:? "Message")
      )

instance Prelude.Hashable InstanceStatusReason

instance Prelude.NFData InstanceStatusReason
