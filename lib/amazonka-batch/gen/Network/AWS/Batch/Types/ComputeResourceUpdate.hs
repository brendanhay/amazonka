{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeResourceUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeResourceUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the attributes of a compute environment that can be updated.
--
--
--
-- /See:/ 'computeResourceUpdate' smart constructor.
data ComputeResourceUpdate = ComputeResourceUpdate'
  { _cruMinvCPUs ::
      !(Maybe Int),
    _cruMaxvCPUs :: !(Maybe Int),
    _cruDesiredvCPUs :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComputeResourceUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cruMinvCPUs' - The minimum number of Amazon EC2 vCPUs that an environment should maintain.
--
-- * 'cruMaxvCPUs' - The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- * 'cruDesiredvCPUs' - The desired number of Amazon EC2 vCPUS in the compute environment.
computeResourceUpdate ::
  ComputeResourceUpdate
computeResourceUpdate =
  ComputeResourceUpdate'
    { _cruMinvCPUs = Nothing,
      _cruMaxvCPUs = Nothing,
      _cruDesiredvCPUs = Nothing
    }

-- | The minimum number of Amazon EC2 vCPUs that an environment should maintain.
cruMinvCPUs :: Lens' ComputeResourceUpdate (Maybe Int)
cruMinvCPUs = lens _cruMinvCPUs (\s a -> s {_cruMinvCPUs = a})

-- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
cruMaxvCPUs :: Lens' ComputeResourceUpdate (Maybe Int)
cruMaxvCPUs = lens _cruMaxvCPUs (\s a -> s {_cruMaxvCPUs = a})

-- | The desired number of Amazon EC2 vCPUS in the compute environment.
cruDesiredvCPUs :: Lens' ComputeResourceUpdate (Maybe Int)
cruDesiredvCPUs = lens _cruDesiredvCPUs (\s a -> s {_cruDesiredvCPUs = a})

instance Hashable ComputeResourceUpdate

instance NFData ComputeResourceUpdate

instance ToJSON ComputeResourceUpdate where
  toJSON ComputeResourceUpdate' {..} =
    object
      ( catMaybes
          [ ("minvCpus" .=) <$> _cruMinvCPUs,
            ("maxvCpus" .=) <$> _cruMaxvCPUs,
            ("desiredvCpus" .=) <$> _cruDesiredvCPUs
          ]
      )
