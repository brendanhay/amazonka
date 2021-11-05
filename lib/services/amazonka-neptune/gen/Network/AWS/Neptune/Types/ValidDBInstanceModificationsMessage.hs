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
-- Module      : Network.AWS.Neptune.Types.ValidDBInstanceModificationsMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Neptune.Types.ValidDBInstanceModificationsMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Neptune.Types.ValidStorageOptions
import qualified Network.AWS.Prelude as Prelude

-- | Information about valid modifications that you can make to your DB
-- instance. Contains the result of a successful call to the
-- DescribeValidDBInstanceModifications action. You can use this
-- information when you call ModifyDBInstance.
--
-- /See:/ 'newValidDBInstanceModificationsMessage' smart constructor.
data ValidDBInstanceModificationsMessage = ValidDBInstanceModificationsMessage'
  { -- | Valid storage options for your DB instance.
    storage :: Prelude.Maybe [ValidStorageOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidDBInstanceModificationsMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storage', 'validDBInstanceModificationsMessage_storage' - Valid storage options for your DB instance.
newValidDBInstanceModificationsMessage ::
  ValidDBInstanceModificationsMessage
newValidDBInstanceModificationsMessage =
  ValidDBInstanceModificationsMessage'
    { storage =
        Prelude.Nothing
    }

-- | Valid storage options for your DB instance.
validDBInstanceModificationsMessage_storage :: Lens.Lens' ValidDBInstanceModificationsMessage (Prelude.Maybe [ValidStorageOptions])
validDBInstanceModificationsMessage_storage = Lens.lens (\ValidDBInstanceModificationsMessage' {storage} -> storage) (\s@ValidDBInstanceModificationsMessage' {} a -> s {storage = a} :: ValidDBInstanceModificationsMessage) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromXML
    ValidDBInstanceModificationsMessage
  where
  parseXML x =
    ValidDBInstanceModificationsMessage'
      Prelude.<$> ( x Core..@? "Storage" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ValidStorageOptions")
                  )

instance
  Prelude.Hashable
    ValidDBInstanceModificationsMessage

instance
  Prelude.NFData
    ValidDBInstanceModificationsMessage
