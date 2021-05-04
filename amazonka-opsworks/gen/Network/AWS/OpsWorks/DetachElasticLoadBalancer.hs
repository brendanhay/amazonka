{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.OpsWorks.DetachElasticLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a specified Elastic Load Balancing instance from its layer.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DetachElasticLoadBalancer
  ( -- * Creating a Request
    DetachElasticLoadBalancer (..),
    newDetachElasticLoadBalancer,

    -- * Request Lenses
    detachElasticLoadBalancer_elasticLoadBalancerName,
    detachElasticLoadBalancer_layerId,

    -- * Destructuring the Response
    DetachElasticLoadBalancerResponse (..),
    newDetachElasticLoadBalancerResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachElasticLoadBalancer' smart constructor.
data DetachElasticLoadBalancer = DetachElasticLoadBalancer'
  { -- | The Elastic Load Balancing instance\'s name.
    elasticLoadBalancerName :: Prelude.Text,
    -- | The ID of the layer that the Elastic Load Balancing instance is attached
    -- to.
    layerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachElasticLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticLoadBalancerName', 'detachElasticLoadBalancer_elasticLoadBalancerName' - The Elastic Load Balancing instance\'s name.
--
-- 'layerId', 'detachElasticLoadBalancer_layerId' - The ID of the layer that the Elastic Load Balancing instance is attached
-- to.
newDetachElasticLoadBalancer ::
  -- | 'elasticLoadBalancerName'
  Prelude.Text ->
  -- | 'layerId'
  Prelude.Text ->
  DetachElasticLoadBalancer
newDetachElasticLoadBalancer
  pElasticLoadBalancerName_
  pLayerId_ =
    DetachElasticLoadBalancer'
      { elasticLoadBalancerName =
          pElasticLoadBalancerName_,
        layerId = pLayerId_
      }

-- | The Elastic Load Balancing instance\'s name.
detachElasticLoadBalancer_elasticLoadBalancerName :: Lens.Lens' DetachElasticLoadBalancer Prelude.Text
detachElasticLoadBalancer_elasticLoadBalancerName = Lens.lens (\DetachElasticLoadBalancer' {elasticLoadBalancerName} -> elasticLoadBalancerName) (\s@DetachElasticLoadBalancer' {} a -> s {elasticLoadBalancerName = a} :: DetachElasticLoadBalancer)

-- | The ID of the layer that the Elastic Load Balancing instance is attached
-- to.
detachElasticLoadBalancer_layerId :: Lens.Lens' DetachElasticLoadBalancer Prelude.Text
detachElasticLoadBalancer_layerId = Lens.lens (\DetachElasticLoadBalancer' {layerId} -> layerId) (\s@DetachElasticLoadBalancer' {} a -> s {layerId = a} :: DetachElasticLoadBalancer)

instance Prelude.AWSRequest DetachElasticLoadBalancer where
  type
    Rs DetachElasticLoadBalancer =
      DetachElasticLoadBalancerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DetachElasticLoadBalancerResponse'

instance Prelude.Hashable DetachElasticLoadBalancer

instance Prelude.NFData DetachElasticLoadBalancer

instance Prelude.ToHeaders DetachElasticLoadBalancer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.DetachElasticLoadBalancer" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DetachElasticLoadBalancer where
  toJSON DetachElasticLoadBalancer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ElasticLoadBalancerName"
                  Prelude..= elasticLoadBalancerName
              ),
            Prelude.Just ("LayerId" Prelude..= layerId)
          ]
      )

instance Prelude.ToPath DetachElasticLoadBalancer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetachElasticLoadBalancer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachElasticLoadBalancerResponse' smart constructor.
data DetachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachElasticLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachElasticLoadBalancerResponse ::
  DetachElasticLoadBalancerResponse
newDetachElasticLoadBalancerResponse =
  DetachElasticLoadBalancerResponse'

instance
  Prelude.NFData
    DetachElasticLoadBalancerResponse
