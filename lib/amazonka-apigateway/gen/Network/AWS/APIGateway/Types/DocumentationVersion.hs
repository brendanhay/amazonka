{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DocumentationVersion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A snapshot of the documentation of an API.
--
--
-- Publishing API documentation involves creating a documentation version associated with an API stage and exporting the versioned documentation to an external (e.g., OpenAPI) file.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationPart' , 'DocumentationVersions'
--
-- /See:/ 'documentationVersion' smart constructor.
data DocumentationVersion = DocumentationVersion'
  { _dvCreatedDate ::
      !(Maybe POSIX),
    _dvVersion :: !(Maybe Text),
    _dvDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvCreatedDate' - The date when the API documentation snapshot is created.
--
-- * 'dvVersion' - The version identifier of the API documentation snapshot.
--
-- * 'dvDescription' - The description of the API documentation snapshot.
documentationVersion ::
  DocumentationVersion
documentationVersion =
  DocumentationVersion'
    { _dvCreatedDate = Nothing,
      _dvVersion = Nothing,
      _dvDescription = Nothing
    }

-- | The date when the API documentation snapshot is created.
dvCreatedDate :: Lens' DocumentationVersion (Maybe UTCTime)
dvCreatedDate = lens _dvCreatedDate (\s a -> s {_dvCreatedDate = a}) . mapping _Time

-- | The version identifier of the API documentation snapshot.
dvVersion :: Lens' DocumentationVersion (Maybe Text)
dvVersion = lens _dvVersion (\s a -> s {_dvVersion = a})

-- | The description of the API documentation snapshot.
dvDescription :: Lens' DocumentationVersion (Maybe Text)
dvDescription = lens _dvDescription (\s a -> s {_dvDescription = a})

instance FromJSON DocumentationVersion where
  parseJSON =
    withObject
      "DocumentationVersion"
      ( \x ->
          DocumentationVersion'
            <$> (x .:? "createdDate")
            <*> (x .:? "version")
            <*> (x .:? "description")
      )

instance Hashable DocumentationVersion

instance NFData DocumentationVersion
