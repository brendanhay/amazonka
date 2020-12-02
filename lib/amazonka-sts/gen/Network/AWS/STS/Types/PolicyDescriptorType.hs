{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types.PolicyDescriptorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.STS.Types.PolicyDescriptorType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A reference to the IAM managed policy that is passed as a session policy for a role session or a federated user session.
--
--
--
-- /See:/ 'policyDescriptorType' smart constructor.
newtype PolicyDescriptorType = PolicyDescriptorType'
  { _pdtArn ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyDescriptorType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdtArn' - The Amazon Resource Name (ARN) of the IAM managed policy to use as a session policy for the role. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
policyDescriptorType ::
  PolicyDescriptorType
policyDescriptorType = PolicyDescriptorType' {_pdtArn = Nothing}

-- | The Amazon Resource Name (ARN) of the IAM managed policy to use as a session policy for the role. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
pdtArn :: Lens' PolicyDescriptorType (Maybe Text)
pdtArn = lens _pdtArn (\s a -> s {_pdtArn = a})

instance Hashable PolicyDescriptorType

instance NFData PolicyDescriptorType

instance ToQuery PolicyDescriptorType where
  toQuery PolicyDescriptorType' {..} = mconcat ["arn" =: _pdtArn]
