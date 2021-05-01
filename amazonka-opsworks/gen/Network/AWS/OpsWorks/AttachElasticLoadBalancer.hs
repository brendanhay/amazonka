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
-- Module      : Network.AWS.OpsWorks.AttachElasticLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.OpsWorks.AttachElasticLoadBalancer
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachElasticLoadBalancer' smart constructor.
data AttachElasticLoadBalancer = AttachElasticLoadBalancer'
  { -- | The Elastic Load Balancing instance\'s name.
    elasticLoadBalancerName :: Prelude.Text,
    -- | The ID of the layer to which the Elastic Load Balancing instance is to
    -- be attached.
    layerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest AttachElasticLoadBalancer where
  type
    Rs AttachElasticLoadBalancer =
      AttachElasticLoadBalancerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      AttachElasticLoadBalancerResponse'

instance Prelude.Hashable AttachElasticLoadBalancer

instance Prelude.NFData AttachElasticLoadBalancer

instance Prelude.ToHeaders AttachElasticLoadBalancer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.AttachElasticLoadBalancer" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AttachElasticLoadBalancer where
  toJSON AttachElasticLoadBalancer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ElasticLoadBalancerName"
                  Prelude..= elasticLoadBalancerName
              ),
            Prelude.Just ("LayerId" Prelude..= layerId)
          ]
      )

instance Prelude.ToPath AttachElasticLoadBalancer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AttachElasticLoadBalancer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachElasticLoadBalancerResponse' smart constructor.
data AttachElasticLoadBalancerResponse = AttachElasticLoadBalancerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
