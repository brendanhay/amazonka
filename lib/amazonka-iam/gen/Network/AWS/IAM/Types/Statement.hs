{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Statement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Statement where

import Network.AWS.IAM.Types.PolicySourceType
import Network.AWS.IAM.Types.Position
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a reference to a @Statement@ element in a policy document that determines the result of the simulation.
--
--
-- This data type is used by the @MatchedStatements@ member of the @'EvaluationResult' @ type.
--
--
-- /See:/ 'statement' smart constructor.
data Statement = Statement'
  { _sSourcePolicyType ::
      !(Maybe PolicySourceType),
    _sSourcePolicyId :: !(Maybe Text),
    _sEndPosition :: !(Maybe Position),
    _sStartPosition :: !(Maybe Position)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Statement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSourcePolicyType' - The type of the policy.
--
-- * 'sSourcePolicyId' - The identifier of the policy that was provided as an input.
--
-- * 'sEndPosition' - The row and column of the end of a @Statement@ in an IAM policy.
--
-- * 'sStartPosition' - The row and column of the beginning of the @Statement@ in an IAM policy.
statement ::
  Statement
statement =
  Statement'
    { _sSourcePolicyType = Nothing,
      _sSourcePolicyId = Nothing,
      _sEndPosition = Nothing,
      _sStartPosition = Nothing
    }

-- | The type of the policy.
sSourcePolicyType :: Lens' Statement (Maybe PolicySourceType)
sSourcePolicyType = lens _sSourcePolicyType (\s a -> s {_sSourcePolicyType = a})

-- | The identifier of the policy that was provided as an input.
sSourcePolicyId :: Lens' Statement (Maybe Text)
sSourcePolicyId = lens _sSourcePolicyId (\s a -> s {_sSourcePolicyId = a})

-- | The row and column of the end of a @Statement@ in an IAM policy.
sEndPosition :: Lens' Statement (Maybe Position)
sEndPosition = lens _sEndPosition (\s a -> s {_sEndPosition = a})

-- | The row and column of the beginning of the @Statement@ in an IAM policy.
sStartPosition :: Lens' Statement (Maybe Position)
sStartPosition = lens _sStartPosition (\s a -> s {_sStartPosition = a})

instance FromXML Statement where
  parseXML x =
    Statement'
      <$> (x .@? "SourcePolicyType")
      <*> (x .@? "SourcePolicyId")
      <*> (x .@? "EndPosition")
      <*> (x .@? "StartPosition")

instance Hashable Statement

instance NFData Statement
