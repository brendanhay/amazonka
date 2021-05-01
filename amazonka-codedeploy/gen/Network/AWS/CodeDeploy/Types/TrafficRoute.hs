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
-- Module      : Network.AWS.CodeDeploy.Types.TrafficRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TrafficRoute where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a listener. The listener contains the path used to
-- route traffic that is received from the load balancer to a target group.
--
-- /See:/ 'newTrafficRoute' smart constructor.
data TrafficRoute = TrafficRoute'
  { -- | The Amazon Resource Name (ARN) of one listener. The listener identifies
    -- the route between a target group and a load balancer. This is an array
    -- of strings with a maximum size of one.
    listenerArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrafficRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerArns', 'trafficRoute_listenerArns' - The Amazon Resource Name (ARN) of one listener. The listener identifies
-- the route between a target group and a load balancer. This is an array
-- of strings with a maximum size of one.
newTrafficRoute ::
  TrafficRoute
newTrafficRoute =
  TrafficRoute' {listenerArns = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of one listener. The listener identifies
-- the route between a target group and a load balancer. This is an array
-- of strings with a maximum size of one.
trafficRoute_listenerArns :: Lens.Lens' TrafficRoute (Prelude.Maybe [Prelude.Text])
trafficRoute_listenerArns = Lens.lens (\TrafficRoute' {listenerArns} -> listenerArns) (\s@TrafficRoute' {} a -> s {listenerArns = a} :: TrafficRoute) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON TrafficRoute where
  parseJSON =
    Prelude.withObject
      "TrafficRoute"
      ( \x ->
          TrafficRoute'
            Prelude.<$> ( x Prelude..:? "listenerArns"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TrafficRoute

instance Prelude.NFData TrafficRoute

instance Prelude.ToJSON TrafficRoute where
  toJSON TrafficRoute' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("listenerArns" Prelude..=)
              Prelude.<$> listenerArns
          ]
      )
