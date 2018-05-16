{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateRepositoryDescription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets or changes the comment or description for a repository.
--
--
module Network.AWS.CodeCommit.UpdateRepositoryDescription
    (
    -- * Creating a Request
      updateRepositoryDescription
    , UpdateRepositoryDescription
    -- * Request Lenses
    , urdRepositoryDescription
    , urdRepositoryName

    -- * Destructuring the Response
    , updateRepositoryDescriptionResponse
    , UpdateRepositoryDescriptionResponse
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of an update repository description operation.
--
--
--
-- /See:/ 'updateRepositoryDescription' smart constructor.
data UpdateRepositoryDescription = UpdateRepositoryDescription'
  { _urdRepositoryDescription :: !(Maybe Text)
  , _urdRepositoryName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRepositoryDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urdRepositoryDescription' - The new comment or description for the specified repository. Repository descriptions are limited to 1,000 characters.
--
-- * 'urdRepositoryName' - The name of the repository to set or change the comment or description for.
updateRepositoryDescription
    :: Text -- ^ 'urdRepositoryName'
    -> UpdateRepositoryDescription
updateRepositoryDescription pRepositoryName_ =
  UpdateRepositoryDescription'
    {_urdRepositoryDescription = Nothing, _urdRepositoryName = pRepositoryName_}


-- | The new comment or description for the specified repository. Repository descriptions are limited to 1,000 characters.
urdRepositoryDescription :: Lens' UpdateRepositoryDescription (Maybe Text)
urdRepositoryDescription = lens _urdRepositoryDescription (\ s a -> s{_urdRepositoryDescription = a})

-- | The name of the repository to set or change the comment or description for.
urdRepositoryName :: Lens' UpdateRepositoryDescription Text
urdRepositoryName = lens _urdRepositoryName (\ s a -> s{_urdRepositoryName = a})

instance AWSRequest UpdateRepositoryDescription where
        type Rs UpdateRepositoryDescription =
             UpdateRepositoryDescriptionResponse
        request = postJSON codeCommit
        response
          = receiveNull UpdateRepositoryDescriptionResponse'

instance Hashable UpdateRepositoryDescription where

instance NFData UpdateRepositoryDescription where

instance ToHeaders UpdateRepositoryDescription where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.UpdateRepositoryDescription" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRepositoryDescription where
        toJSON UpdateRepositoryDescription'{..}
          = object
              (catMaybes
                 [("repositoryDescription" .=) <$>
                    _urdRepositoryDescription,
                  Just ("repositoryName" .= _urdRepositoryName)])

instance ToPath UpdateRepositoryDescription where
        toPath = const "/"

instance ToQuery UpdateRepositoryDescription where
        toQuery = const mempty

-- | /See:/ 'updateRepositoryDescriptionResponse' smart constructor.
data UpdateRepositoryDescriptionResponse =
  UpdateRepositoryDescriptionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRepositoryDescriptionResponse' with the minimum fields required to make a request.
--
updateRepositoryDescriptionResponse
    :: UpdateRepositoryDescriptionResponse
updateRepositoryDescriptionResponse = UpdateRepositoryDescriptionResponse'


instance NFData UpdateRepositoryDescriptionResponse
         where
