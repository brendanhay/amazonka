{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.GitConfigForUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.GitConfigForUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies configuration details for a Git repository when the repository is updated.
--
--
--
-- /See:/ 'gitConfigForUpdate' smart constructor.
newtype GitConfigForUpdate = GitConfigForUpdate'
  { _gcfuSecretARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GitConfigForUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfuSecretARN' - The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format: @{"username": /UserName/ , "password": /Password/ }@
gitConfigForUpdate ::
  GitConfigForUpdate
gitConfigForUpdate = GitConfigForUpdate' {_gcfuSecretARN = Nothing}

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format: @{"username": /UserName/ , "password": /Password/ }@
gcfuSecretARN :: Lens' GitConfigForUpdate (Maybe Text)
gcfuSecretARN = lens _gcfuSecretARN (\s a -> s {_gcfuSecretARN = a})

instance Hashable GitConfigForUpdate

instance NFData GitConfigForUpdate

instance ToJSON GitConfigForUpdate where
  toJSON GitConfigForUpdate' {..} =
    object (catMaybes [("SecretArn" .=) <$> _gcfuSecretARN])
