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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerListenerDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerListenerDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerListener

-- | Lists the policies that are enabled for a load balancer listener.
--
-- /See:/ 'newAwsElbLoadBalancerListenerDescription' smart constructor.
data AwsElbLoadBalancerListenerDescription = AwsElbLoadBalancerListenerDescription'
  { -- | Information about the listener.
    listener :: Prelude.Maybe AwsElbLoadBalancerListener,
    -- | The policies enabled for the listener.
    policyNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerListenerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listener', 'awsElbLoadBalancerListenerDescription_listener' - Information about the listener.
--
-- 'policyNames', 'awsElbLoadBalancerListenerDescription_policyNames' - The policies enabled for the listener.
newAwsElbLoadBalancerListenerDescription ::
  AwsElbLoadBalancerListenerDescription
newAwsElbLoadBalancerListenerDescription =
  AwsElbLoadBalancerListenerDescription'
    { listener =
        Prelude.Nothing,
      policyNames = Prelude.Nothing
    }

-- | Information about the listener.
awsElbLoadBalancerListenerDescription_listener :: Lens.Lens' AwsElbLoadBalancerListenerDescription (Prelude.Maybe AwsElbLoadBalancerListener)
awsElbLoadBalancerListenerDescription_listener = Lens.lens (\AwsElbLoadBalancerListenerDescription' {listener} -> listener) (\s@AwsElbLoadBalancerListenerDescription' {} a -> s {listener = a} :: AwsElbLoadBalancerListenerDescription)

-- | The policies enabled for the listener.
awsElbLoadBalancerListenerDescription_policyNames :: Lens.Lens' AwsElbLoadBalancerListenerDescription (Prelude.Maybe [Prelude.Text])
awsElbLoadBalancerListenerDescription_policyNames = Lens.lens (\AwsElbLoadBalancerListenerDescription' {policyNames} -> policyNames) (\s@AwsElbLoadBalancerListenerDescription' {} a -> s {policyNames = a} :: AwsElbLoadBalancerListenerDescription) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsElbLoadBalancerListenerDescription
  where
  parseJSON =
    Data.withObject
      "AwsElbLoadBalancerListenerDescription"
      ( \x ->
          AwsElbLoadBalancerListenerDescription'
            Prelude.<$> (x Data..:? "Listener")
            Prelude.<*> (x Data..:? "PolicyNames" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsElbLoadBalancerListenerDescription
  where
  hashWithSalt
    _salt
    AwsElbLoadBalancerListenerDescription' {..} =
      _salt
        `Prelude.hashWithSalt` listener
        `Prelude.hashWithSalt` policyNames

instance
  Prelude.NFData
    AwsElbLoadBalancerListenerDescription
  where
  rnf AwsElbLoadBalancerListenerDescription' {..} =
    Prelude.rnf listener
      `Prelude.seq` Prelude.rnf policyNames

instance
  Data.ToJSON
    AwsElbLoadBalancerListenerDescription
  where
  toJSON AwsElbLoadBalancerListenerDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Listener" Data..=) Prelude.<$> listener,
            ("PolicyNames" Data..=) Prelude.<$> policyNames
          ]
      )
