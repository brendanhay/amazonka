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
-- Module      : Amazonka.AppMesh.Types.AwsCloudMapServiceDiscovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.AwsCloudMapServiceDiscovery where

import Amazonka.AppMesh.Types.AwsCloudMapInstanceAttribute
import Amazonka.AppMesh.Types.IpPreference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the Cloud Map service discovery information
-- for your virtual node.
--
-- Cloud Map is not available in the eu-south-1 Region.
--
-- /See:/ 'newAwsCloudMapServiceDiscovery' smart constructor.
data AwsCloudMapServiceDiscovery = AwsCloudMapServiceDiscovery'
  { -- | A string map that contains attributes with values that you can use to
    -- filter instances by any custom attribute that you specified when you
    -- registered the instance. Only instances that match all of the specified
    -- key\/value pairs will be returned.
    attributes :: Prelude.Maybe [AwsCloudMapInstanceAttribute],
    -- | The preferred IP version that this virtual node uses. Setting the IP
    -- preference on the virtual node only overrides the IP preference set for
    -- the mesh on this specific node.
    ipPreference :: Prelude.Maybe IpPreference,
    -- | The name of the Cloud Map namespace to use.
    namespaceName :: Prelude.Text,
    -- | The name of the Cloud Map service to use.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudMapServiceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'awsCloudMapServiceDiscovery_attributes' - A string map that contains attributes with values that you can use to
-- filter instances by any custom attribute that you specified when you
-- registered the instance. Only instances that match all of the specified
-- key\/value pairs will be returned.
--
-- 'ipPreference', 'awsCloudMapServiceDiscovery_ipPreference' - The preferred IP version that this virtual node uses. Setting the IP
-- preference on the virtual node only overrides the IP preference set for
-- the mesh on this specific node.
--
-- 'namespaceName', 'awsCloudMapServiceDiscovery_namespaceName' - The name of the Cloud Map namespace to use.
--
-- 'serviceName', 'awsCloudMapServiceDiscovery_serviceName' - The name of the Cloud Map service to use.
newAwsCloudMapServiceDiscovery ::
  -- | 'namespaceName'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  AwsCloudMapServiceDiscovery
newAwsCloudMapServiceDiscovery
  pNamespaceName_
  pServiceName_ =
    AwsCloudMapServiceDiscovery'
      { attributes =
          Prelude.Nothing,
        ipPreference = Prelude.Nothing,
        namespaceName = pNamespaceName_,
        serviceName = pServiceName_
      }

-- | A string map that contains attributes with values that you can use to
-- filter instances by any custom attribute that you specified when you
-- registered the instance. Only instances that match all of the specified
-- key\/value pairs will be returned.
awsCloudMapServiceDiscovery_attributes :: Lens.Lens' AwsCloudMapServiceDiscovery (Prelude.Maybe [AwsCloudMapInstanceAttribute])
awsCloudMapServiceDiscovery_attributes = Lens.lens (\AwsCloudMapServiceDiscovery' {attributes} -> attributes) (\s@AwsCloudMapServiceDiscovery' {} a -> s {attributes = a} :: AwsCloudMapServiceDiscovery) Prelude.. Lens.mapping Lens.coerced

-- | The preferred IP version that this virtual node uses. Setting the IP
-- preference on the virtual node only overrides the IP preference set for
-- the mesh on this specific node.
awsCloudMapServiceDiscovery_ipPreference :: Lens.Lens' AwsCloudMapServiceDiscovery (Prelude.Maybe IpPreference)
awsCloudMapServiceDiscovery_ipPreference = Lens.lens (\AwsCloudMapServiceDiscovery' {ipPreference} -> ipPreference) (\s@AwsCloudMapServiceDiscovery' {} a -> s {ipPreference = a} :: AwsCloudMapServiceDiscovery)

-- | The name of the Cloud Map namespace to use.
awsCloudMapServiceDiscovery_namespaceName :: Lens.Lens' AwsCloudMapServiceDiscovery Prelude.Text
awsCloudMapServiceDiscovery_namespaceName = Lens.lens (\AwsCloudMapServiceDiscovery' {namespaceName} -> namespaceName) (\s@AwsCloudMapServiceDiscovery' {} a -> s {namespaceName = a} :: AwsCloudMapServiceDiscovery)

-- | The name of the Cloud Map service to use.
awsCloudMapServiceDiscovery_serviceName :: Lens.Lens' AwsCloudMapServiceDiscovery Prelude.Text
awsCloudMapServiceDiscovery_serviceName = Lens.lens (\AwsCloudMapServiceDiscovery' {serviceName} -> serviceName) (\s@AwsCloudMapServiceDiscovery' {} a -> s {serviceName = a} :: AwsCloudMapServiceDiscovery)

instance Data.FromJSON AwsCloudMapServiceDiscovery where
  parseJSON =
    Data.withObject
      "AwsCloudMapServiceDiscovery"
      ( \x ->
          AwsCloudMapServiceDiscovery'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ipPreference")
            Prelude.<*> (x Data..: "namespaceName")
            Prelude.<*> (x Data..: "serviceName")
      )

instance Prelude.Hashable AwsCloudMapServiceDiscovery where
  hashWithSalt _salt AwsCloudMapServiceDiscovery' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` ipPreference
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData AwsCloudMapServiceDiscovery where
  rnf AwsCloudMapServiceDiscovery' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf ipPreference
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToJSON AwsCloudMapServiceDiscovery where
  toJSON AwsCloudMapServiceDiscovery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("ipPreference" Data..=) Prelude.<$> ipPreference,
            Prelude.Just ("namespaceName" Data..= namespaceName),
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )
