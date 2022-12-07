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
-- Module      : Amazonka.IAM.Types.EntityDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.EntityDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.EntityInfo
import qualified Amazonka.Prelude as Prelude

-- | An object that contains details about when the IAM entities (users or
-- roles) were last used in an attempt to access the specified Amazon Web
-- Services service.
--
-- This data type is a response element in the
-- GetServiceLastAccessedDetailsWithEntities operation.
--
-- /See:/ 'newEntityDetails' smart constructor.
data EntityDetails = EntityDetails'
  { -- | The date and time,
    -- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- authenticated entity last attempted to access Amazon Web Services.
    -- Amazon Web Services does not report unauthenticated requests.
    --
    -- This field is null if no IAM entities attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    lastAuthenticated :: Prelude.Maybe Data.ISO8601,
    -- | The @EntityInfo@ object that contains details about the entity (user or
    -- role).
    entityInfo :: EntityInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastAuthenticated', 'entityDetails_lastAuthenticated' - The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- authenticated entity last attempted to access Amazon Web Services.
-- Amazon Web Services does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
--
-- 'entityInfo', 'entityDetails_entityInfo' - The @EntityInfo@ object that contains details about the entity (user or
-- role).
newEntityDetails ::
  -- | 'entityInfo'
  EntityInfo ->
  EntityDetails
newEntityDetails pEntityInfo_ =
  EntityDetails'
    { lastAuthenticated = Prelude.Nothing,
      entityInfo = pEntityInfo_
    }

-- | The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- authenticated entity last attempted to access Amazon Web Services.
-- Amazon Web Services does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
entityDetails_lastAuthenticated :: Lens.Lens' EntityDetails (Prelude.Maybe Prelude.UTCTime)
entityDetails_lastAuthenticated = Lens.lens (\EntityDetails' {lastAuthenticated} -> lastAuthenticated) (\s@EntityDetails' {} a -> s {lastAuthenticated = a} :: EntityDetails) Prelude.. Lens.mapping Data._Time

-- | The @EntityInfo@ object that contains details about the entity (user or
-- role).
entityDetails_entityInfo :: Lens.Lens' EntityDetails EntityInfo
entityDetails_entityInfo = Lens.lens (\EntityDetails' {entityInfo} -> entityInfo) (\s@EntityDetails' {} a -> s {entityInfo = a} :: EntityDetails)

instance Data.FromXML EntityDetails where
  parseXML x =
    EntityDetails'
      Prelude.<$> (x Data..@? "LastAuthenticated")
      Prelude.<*> (x Data..@ "EntityInfo")

instance Prelude.Hashable EntityDetails where
  hashWithSalt _salt EntityDetails' {..} =
    _salt `Prelude.hashWithSalt` lastAuthenticated
      `Prelude.hashWithSalt` entityInfo

instance Prelude.NFData EntityDetails where
  rnf EntityDetails' {..} =
    Prelude.rnf lastAuthenticated
      `Prelude.seq` Prelude.rnf entityInfo
