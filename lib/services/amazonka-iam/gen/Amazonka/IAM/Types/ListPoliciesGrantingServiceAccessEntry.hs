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
-- Module      : Amazonka.IAM.Types.ListPoliciesGrantingServiceAccessEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.ListPoliciesGrantingServiceAccessEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.PolicyGrantingServiceAccess
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the permissions policies that are attached to the
-- specified identity (user, group, or role).
--
-- This data type is used as a response element in the
-- ListPoliciesGrantingServiceAccess operation.
--
-- /See:/ 'newListPoliciesGrantingServiceAccessEntry' smart constructor.
data ListPoliciesGrantingServiceAccessEntry = ListPoliciesGrantingServiceAccessEntry'
  { -- | The namespace of the service that was accessed.
    --
    -- To learn the service namespace of a service, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
    -- in the /Service Authorization Reference/. Choose the name of the service
    -- to view details for that service. In the first paragraph, find the
    -- service prefix. For example, @(service prefix: a4b)@. For more
    -- information about service namespaces, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces Amazon Web Services service namespaces>
    -- in the /Amazon Web Services General Reference/.
    serviceNamespace :: Prelude.Maybe Prelude.Text,
    -- | The @PoliciesGrantingServiceAccess@ object that contains details about
    -- the policy.
    policies :: Prelude.Maybe [PolicyGrantingServiceAccess]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPoliciesGrantingServiceAccessEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNamespace', 'listPoliciesGrantingServiceAccessEntry_serviceNamespace' - The namespace of the service that was accessed.
--
-- To learn the service namespace of a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
-- in the /Service Authorization Reference/. Choose the name of the service
-- to view details for that service. In the first paragraph, find the
-- service prefix. For example, @(service prefix: a4b)@. For more
-- information about service namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces Amazon Web Services service namespaces>
-- in the /Amazon Web Services General Reference/.
--
-- 'policies', 'listPoliciesGrantingServiceAccessEntry_policies' - The @PoliciesGrantingServiceAccess@ object that contains details about
-- the policy.
newListPoliciesGrantingServiceAccessEntry ::
  ListPoliciesGrantingServiceAccessEntry
newListPoliciesGrantingServiceAccessEntry =
  ListPoliciesGrantingServiceAccessEntry'
    { serviceNamespace =
        Prelude.Nothing,
      policies = Prelude.Nothing
    }

-- | The namespace of the service that was accessed.
--
-- To learn the service namespace of a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
-- in the /Service Authorization Reference/. Choose the name of the service
-- to view details for that service. In the first paragraph, find the
-- service prefix. For example, @(service prefix: a4b)@. For more
-- information about service namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces Amazon Web Services service namespaces>
-- in the /Amazon Web Services General Reference/.
listPoliciesGrantingServiceAccessEntry_serviceNamespace :: Lens.Lens' ListPoliciesGrantingServiceAccessEntry (Prelude.Maybe Prelude.Text)
listPoliciesGrantingServiceAccessEntry_serviceNamespace = Lens.lens (\ListPoliciesGrantingServiceAccessEntry' {serviceNamespace} -> serviceNamespace) (\s@ListPoliciesGrantingServiceAccessEntry' {} a -> s {serviceNamespace = a} :: ListPoliciesGrantingServiceAccessEntry)

-- | The @PoliciesGrantingServiceAccess@ object that contains details about
-- the policy.
listPoliciesGrantingServiceAccessEntry_policies :: Lens.Lens' ListPoliciesGrantingServiceAccessEntry (Prelude.Maybe [PolicyGrantingServiceAccess])
listPoliciesGrantingServiceAccessEntry_policies = Lens.lens (\ListPoliciesGrantingServiceAccessEntry' {policies} -> policies) (\s@ListPoliciesGrantingServiceAccessEntry' {} a -> s {policies = a} :: ListPoliciesGrantingServiceAccessEntry) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromXML
    ListPoliciesGrantingServiceAccessEntry
  where
  parseXML x =
    ListPoliciesGrantingServiceAccessEntry'
      Prelude.<$> (x Data..@? "ServiceNamespace")
      Prelude.<*> ( x Data..@? "Policies" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance
  Prelude.Hashable
    ListPoliciesGrantingServiceAccessEntry
  where
  hashWithSalt
    _salt
    ListPoliciesGrantingServiceAccessEntry' {..} =
      _salt `Prelude.hashWithSalt` serviceNamespace
        `Prelude.hashWithSalt` policies

instance
  Prelude.NFData
    ListPoliciesGrantingServiceAccessEntry
  where
  rnf ListPoliciesGrantingServiceAccessEntry' {..} =
    Prelude.rnf serviceNamespace
      `Prelude.seq` Prelude.rnf policies
