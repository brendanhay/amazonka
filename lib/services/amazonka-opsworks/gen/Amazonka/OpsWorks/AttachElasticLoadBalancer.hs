{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.AttachElasticLoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an Elastic Load Balancing load balancer to a specified layer.
-- AWS OpsWorks Stacks does not support Application Load Balancer. You can
-- only use Classic Load Balancer with AWS OpsWorks Stacks. For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/layers-elb.html Elastic Load Balancing>.
--
-- You must create the Elastic Load Balancing instance separately, by using
-- the Elastic Load Balancing console, API, or CLI. For more information,
-- see
-- <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/Welcome.html Elastic Load Balancing Developer Guide>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.AttachElasticLoadBalancer
  ( -- * Creating a Request
    AttachElasticLoadBalancer (..),
    newAttachElasticLoadBalancer,

    -- * Request Lenses
    attachElasticLoadBalancer_elasticLoadBalancerName,
    attachElasticLoadBalancer_layerId,

    -- * Destructuring the Response
    AttachElasticLoadBalancerResponse (..),
    newAttachElasticLoadBalancerResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachElasticLoadBalancer' smart constructor.
data AttachElasticLoadBalancer = AttachElasticLoadBalancer'
  { -- | The Elastic Load Balancing instance\'s name.
    elasticLoadBalancerName :: Prelude.Text,
    -- | The ID of the layer to which the Elastic Load Balancing instance is to
    -- be attached.
    layerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachElasticLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticLoadBalancerName', 'attachElasticLoadBalancer_elasticLoadBalancerName' - The Elastic Load Balancing instance\'s name.
--
-- 'layerId', 'attachElasticLoadBalancer_layerId' - The ID of the layer to which the Elastic Load Balancing instance is to
-- be attached.
newAttachElasticLoadBalancer ::
  -- | 'elasticLoadBalancerName'
  Prelude.Text ->
  -- | 'layerId'
  Prelude.Text ->
  AttachElasticLoadBalancer
newAttachElasticLoadBalancer
  pElasticLoadBalancerName_
  pLayerId_ =
    AttachElasticLoadBalancer'
      { elasticLoadBalancerName =
          pElasticLoadBalancerName_,
        layerId = pLayerId_
      }

-- | The Elastic Load Balancing instance\'s name.
attachElasticLoadBalancer_elasticLoadBalancerName :: Lens.Lens' AttachElasticLoadBalancer Prelude.Text
attachElasticLoadBalancer_elasticLoadBalancerName = Lens.lens (\AttachElasticLoadBalancer' {elasticLoadBalancerName} -> elasticLoadBalancerName) (\s@AttachElasticLoadBalancer' {} a -> s {elasticLoadBalancerName = a} :: AttachElasticLoadBalancer)

-- | The ID of the layer to which the Elastic Load Balancing instance is to
-- be attached.
attachElasticLoadBalancer_layerId :: Lens.Lens' AttachElasticLoadBalancer Prelude.Text
attachElasticLoadBalancer_layerId = Lens.lens (\AttachElasticLoadBalancer' {layerId} -> layerId) (\s@AttachElasticLoadBalancer' {} a -> s {layerId = a} :: AttachElasticLoadBalancer)

instance Core.AWSRequest AttachElasticLoadBalancer where
  type
    AWSResponse AttachElasticLoadBalancer =
      AttachElasticLoadBalancerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      AttachElasticLoadBalancerResponse'

instance Prelude.Hashable AttachElasticLoadBalancer where
  hashWithSalt _salt AttachElasticLoadBalancer' {..} =
    _salt
      `Prelude.hashWithSalt` elasticLoadBalancerName
      `Prelude.hashWithSalt` layerId

instance Prelude.NFData AttachElasticLoadBalancer where
  rnf AttachElasticLoadBalancer' {..} =
    Prelude.rnf elasticLoadBalancerName
      `Prelude.seq` Prelude.rnf layerId

instance Data.ToHeaders AttachElasticLoadBalancer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.AttachElasticLoadBalancer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AttachElasticLoadBalancer where
  toJSON AttachElasticLoadBalancer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ElasticLoadBalancerName"
                  Data..= elasticLoadBalancerName
              ),
            Prelude.Just ("LayerId" Data..= layerId)
          ]
      )

instance Data.ToPath AttachElasticLoadBalancer where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachElasticLoadBalancer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachElasticLoadBalancerResponse' smart constructor.
data AttachElasticLoadBalancerResponse = AttachElasticLoadBalancerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachElasticLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAttachElasticLoadBalancerResponse ::
  AttachElasticLoadBalancerResponse
newAttachElasticLoadBalancerResponse =
  AttachElasticLoadBalancerResponse'

instance
  Prelude.NFData
    AttachElasticLoadBalancerResponse
  where
  rnf _ = ()
