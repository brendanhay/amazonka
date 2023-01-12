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
-- Module      : Amazonka.EC2.Types.DeregisterInstanceTagAttributeRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DeregisterInstanceTagAttributeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the tag keys to deregister for the current Region. You
-- can either specify individual tag keys or deregister all tag keys in the
-- current Region. You must specify either @IncludeAllTagsOfInstance@ or
-- @InstanceTagKeys@ in the request
--
-- /See:/ 'newDeregisterInstanceTagAttributeRequest' smart constructor.
data DeregisterInstanceTagAttributeRequest = DeregisterInstanceTagAttributeRequest'
  { -- | Indicates whether to deregister all tag keys in the current Region.
    -- Specify @false@ to deregister all tag keys.
    includeAllTagsOfInstance :: Prelude.Maybe Prelude.Bool,
    -- | Information about the tag keys to deregister.
    instanceTagKeys :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstanceTagAttributeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeAllTagsOfInstance', 'deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance' - Indicates whether to deregister all tag keys in the current Region.
-- Specify @false@ to deregister all tag keys.
--
-- 'instanceTagKeys', 'deregisterInstanceTagAttributeRequest_instanceTagKeys' - Information about the tag keys to deregister.
newDeregisterInstanceTagAttributeRequest ::
  DeregisterInstanceTagAttributeRequest
newDeregisterInstanceTagAttributeRequest =
  DeregisterInstanceTagAttributeRequest'
    { includeAllTagsOfInstance =
        Prelude.Nothing,
      instanceTagKeys = Prelude.Nothing
    }

-- | Indicates whether to deregister all tag keys in the current Region.
-- Specify @false@ to deregister all tag keys.
deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance :: Lens.Lens' DeregisterInstanceTagAttributeRequest (Prelude.Maybe Prelude.Bool)
deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance = Lens.lens (\DeregisterInstanceTagAttributeRequest' {includeAllTagsOfInstance} -> includeAllTagsOfInstance) (\s@DeregisterInstanceTagAttributeRequest' {} a -> s {includeAllTagsOfInstance = a} :: DeregisterInstanceTagAttributeRequest)

-- | Information about the tag keys to deregister.
deregisterInstanceTagAttributeRequest_instanceTagKeys :: Lens.Lens' DeregisterInstanceTagAttributeRequest (Prelude.Maybe [Prelude.Text])
deregisterInstanceTagAttributeRequest_instanceTagKeys = Lens.lens (\DeregisterInstanceTagAttributeRequest' {instanceTagKeys} -> instanceTagKeys) (\s@DeregisterInstanceTagAttributeRequest' {} a -> s {instanceTagKeys = a} :: DeregisterInstanceTagAttributeRequest) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    DeregisterInstanceTagAttributeRequest
  where
  hashWithSalt
    _salt
    DeregisterInstanceTagAttributeRequest' {..} =
      _salt
        `Prelude.hashWithSalt` includeAllTagsOfInstance
        `Prelude.hashWithSalt` instanceTagKeys

instance
  Prelude.NFData
    DeregisterInstanceTagAttributeRequest
  where
  rnf DeregisterInstanceTagAttributeRequest' {..} =
    Prelude.rnf includeAllTagsOfInstance
      `Prelude.seq` Prelude.rnf instanceTagKeys

instance
  Data.ToQuery
    DeregisterInstanceTagAttributeRequest
  where
  toQuery DeregisterInstanceTagAttributeRequest' {..} =
    Prelude.mconcat
      [ "IncludeAllTagsOfInstance"
          Data.=: includeAllTagsOfInstance,
        Data.toQuery
          ( Data.toQueryList "InstanceTagKey"
              Prelude.<$> instanceTagKeys
          )
      ]
