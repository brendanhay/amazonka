{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EnvironmentFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EnvironmentFile where

import Network.AWS.ECS.Types.EnvironmentFileType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of files containing the environment variables to pass to a container. You can specify up to ten environment files. The file must have a @.env@ file extension. Each line in an environment file should contain an environment variable in @VARIABLE=VALUE@ format. Lines beginning with @#@ are treated as comments and are ignored. For more information on the environment variable file syntax, see <https://docs.docker.com/compose/env-file/ Declare default environment variables in file> .
--
--
-- If there are environment variables specified using the @environment@ parameter in a container definition, they take precedence over the variables contained within an environment file. If multiple environment files are specified that contain the same variable, they are processed from the top down. It is recommended to use unique variable names. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- This field is not valid for containers in tasks using the Fargate launch type.
--
--
-- /See:/ 'environmentFile' smart constructor.
data EnvironmentFile = EnvironmentFile'
  { _efValue :: !Text,
    _efType :: !EnvironmentFileType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentFile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efValue' - The Amazon Resource Name (ARN) of the Amazon S3 object containing the environment variable file.
--
-- * 'efType' - The file type to use. The only supported value is @s3@ .
environmentFile ::
  -- | 'efValue'
  Text ->
  -- | 'efType'
  EnvironmentFileType ->
  EnvironmentFile
environmentFile pValue_ pType_ =
  EnvironmentFile' {_efValue = pValue_, _efType = pType_}

-- | The Amazon Resource Name (ARN) of the Amazon S3 object containing the environment variable file.
efValue :: Lens' EnvironmentFile Text
efValue = lens _efValue (\s a -> s {_efValue = a})

-- | The file type to use. The only supported value is @s3@ .
efType :: Lens' EnvironmentFile EnvironmentFileType
efType = lens _efType (\s a -> s {_efType = a})

instance FromJSON EnvironmentFile where
  parseJSON =
    withObject
      "EnvironmentFile"
      (\x -> EnvironmentFile' <$> (x .: "value") <*> (x .: "type"))

instance Hashable EnvironmentFile

instance NFData EnvironmentFile

instance ToJSON EnvironmentFile where
  toJSON EnvironmentFile' {..} =
    object
      (catMaybes [Just ("value" .= _efValue), Just ("type" .= _efType)])
