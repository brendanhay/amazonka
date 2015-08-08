{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateRepositoryName
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renames a repository.
--
-- /See:/ <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_UpdateRepositoryName.html AWS API Reference> for UpdateRepositoryName.
module Network.AWS.CodeCommit.UpdateRepositoryName
    (
    -- * Creating a Request
      UpdateRepositoryName
    , updateRepositoryName
    -- * Request Lenses
    , urnOldName
    , urnNewName

    -- * Destructuring the Response
    , UpdateRepositoryNameResponse
    , updateRepositoryNameResponse
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an update repository description operation.
--
-- /See:/ 'updateRepositoryName' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urnOldName'
--
-- * 'urnNewName'
data UpdateRepositoryName = UpdateRepositoryName'
    { _urnOldName :: !Text
    , _urnNewName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateRepositoryName' smart constructor.
updateRepositoryName :: Text -> Text -> UpdateRepositoryName
updateRepositoryName pOldName_ pNewName_ =
    UpdateRepositoryName'
    { _urnOldName = pOldName_
    , _urnNewName = pNewName_
    }

-- | Undocumented member.
urnOldName :: Lens' UpdateRepositoryName Text
urnOldName = lens _urnOldName (\ s a -> s{_urnOldName = a});

-- | Undocumented member.
urnNewName :: Lens' UpdateRepositoryName Text
urnNewName = lens _urnNewName (\ s a -> s{_urnNewName = a});

instance AWSRequest UpdateRepositoryName where
        type Sv UpdateRepositoryName = CodeCommit
        type Rs UpdateRepositoryName =
             UpdateRepositoryNameResponse
        request = postJSON
        response = receiveNull UpdateRepositoryNameResponse'

instance ToHeaders UpdateRepositoryName where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.UpdateRepositoryName" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRepositoryName where
        toJSON UpdateRepositoryName'{..}
          = object
              ["oldName" .= _urnOldName, "newName" .= _urnNewName]

instance ToPath UpdateRepositoryName where
        toPath = const "/"

instance ToQuery UpdateRepositoryName where
        toQuery = const mempty

-- | /See:/ 'updateRepositoryNameResponse' smart constructor.
data UpdateRepositoryNameResponse =
    UpdateRepositoryNameResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateRepositoryNameResponse' smart constructor.
updateRepositoryNameResponse :: UpdateRepositoryNameResponse
updateRepositoryNameResponse = UpdateRepositoryNameResponse'
