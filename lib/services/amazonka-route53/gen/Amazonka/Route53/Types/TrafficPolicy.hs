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
-- Module      : Amazonka.Route53.Types.TrafficPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.TrafficPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.RRType

-- | A complex type that contains settings for a traffic policy.
--
-- /See:/ 'newTrafficPolicy' smart constructor.
data TrafficPolicy = TrafficPolicy'
  { -- | The comment that you specify in the @CreateTrafficPolicy@ request, if
    -- any.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The ID that Amazon Route 53 assigned to a traffic policy when you
    -- created it.
    id :: Prelude.Text,
    -- | The version number that Amazon Route 53 assigns to a traffic policy. For
    -- a new traffic policy, the value of @Version@ is always 1.
    version :: Prelude.Natural,
    -- | The name that you specified when you created the traffic policy.
    name :: Prelude.Text,
    -- | The DNS type of the resource record sets that Amazon Route 53 creates
    -- when you use a traffic policy to create a traffic policy instance.
    type' :: RRType,
    -- | The definition of a traffic policy in JSON format. You specify the JSON
    -- document to use for a new traffic policy in the @CreateTrafficPolicy@
    -- request. For more information about the JSON format, see
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format>.
    document :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'trafficPolicy_comment' - The comment that you specify in the @CreateTrafficPolicy@ request, if
-- any.
--
-- 'id', 'trafficPolicy_id' - The ID that Amazon Route 53 assigned to a traffic policy when you
-- created it.
--
-- 'version', 'trafficPolicy_version' - The version number that Amazon Route 53 assigns to a traffic policy. For
-- a new traffic policy, the value of @Version@ is always 1.
--
-- 'name', 'trafficPolicy_name' - The name that you specified when you created the traffic policy.
--
-- 'type'', 'trafficPolicy_type' - The DNS type of the resource record sets that Amazon Route 53 creates
-- when you use a traffic policy to create a traffic policy instance.
--
-- 'document', 'trafficPolicy_document' - The definition of a traffic policy in JSON format. You specify the JSON
-- document to use for a new traffic policy in the @CreateTrafficPolicy@
-- request. For more information about the JSON format, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format>.
newTrafficPolicy ::
  -- | 'id'
  Prelude.Text ->
  -- | 'version'
  Prelude.Natural ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  RRType ->
  -- | 'document'
  Prelude.Text ->
  TrafficPolicy
newTrafficPolicy
  pId_
  pVersion_
  pName_
  pType_
  pDocument_ =
    TrafficPolicy'
      { comment = Prelude.Nothing,
        id = pId_,
        version = pVersion_,
        name = pName_,
        type' = pType_,
        document = pDocument_
      }

-- | The comment that you specify in the @CreateTrafficPolicy@ request, if
-- any.
trafficPolicy_comment :: Lens.Lens' TrafficPolicy (Prelude.Maybe Prelude.Text)
trafficPolicy_comment = Lens.lens (\TrafficPolicy' {comment} -> comment) (\s@TrafficPolicy' {} a -> s {comment = a} :: TrafficPolicy)

-- | The ID that Amazon Route 53 assigned to a traffic policy when you
-- created it.
trafficPolicy_id :: Lens.Lens' TrafficPolicy Prelude.Text
trafficPolicy_id = Lens.lens (\TrafficPolicy' {id} -> id) (\s@TrafficPolicy' {} a -> s {id = a} :: TrafficPolicy)

-- | The version number that Amazon Route 53 assigns to a traffic policy. For
-- a new traffic policy, the value of @Version@ is always 1.
trafficPolicy_version :: Lens.Lens' TrafficPolicy Prelude.Natural
trafficPolicy_version = Lens.lens (\TrafficPolicy' {version} -> version) (\s@TrafficPolicy' {} a -> s {version = a} :: TrafficPolicy)

-- | The name that you specified when you created the traffic policy.
trafficPolicy_name :: Lens.Lens' TrafficPolicy Prelude.Text
trafficPolicy_name = Lens.lens (\TrafficPolicy' {name} -> name) (\s@TrafficPolicy' {} a -> s {name = a} :: TrafficPolicy)

-- | The DNS type of the resource record sets that Amazon Route 53 creates
-- when you use a traffic policy to create a traffic policy instance.
trafficPolicy_type :: Lens.Lens' TrafficPolicy RRType
trafficPolicy_type = Lens.lens (\TrafficPolicy' {type'} -> type') (\s@TrafficPolicy' {} a -> s {type' = a} :: TrafficPolicy)

-- | The definition of a traffic policy in JSON format. You specify the JSON
-- document to use for a new traffic policy in the @CreateTrafficPolicy@
-- request. For more information about the JSON format, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format>.
trafficPolicy_document :: Lens.Lens' TrafficPolicy Prelude.Text
trafficPolicy_document = Lens.lens (\TrafficPolicy' {document} -> document) (\s@TrafficPolicy' {} a -> s {document = a} :: TrafficPolicy)

instance Data.FromXML TrafficPolicy where
  parseXML x =
    TrafficPolicy'
      Prelude.<$> (x Data..@? "Comment")
      Prelude.<*> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "Version")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "Type")
      Prelude.<*> (x Data..@ "Document")

instance Prelude.Hashable TrafficPolicy where
  hashWithSalt _salt TrafficPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` document

instance Prelude.NFData TrafficPolicy where
  rnf TrafficPolicy' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf document
