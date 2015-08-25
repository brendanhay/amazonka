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
-- Module      : Network.AWS.CloudSearch.DefineIndexField
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an 'IndexField' for the search domain. Used to create new
-- fields and modify existing ones. You must specify the name of the domain
-- you are configuring and an index field configuration. The index field
-- configuration specifies a unique name, the index field type, and the
-- options you want to configure for the field. The options you can specify
-- depend on the 'IndexFieldType'. If the field exists, the new
-- configuration replaces the old one. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineIndexField.html AWS API Reference> for DefineIndexField.
module Network.AWS.CloudSearch.DefineIndexField
    (
    -- * Creating a Request
      defineIndexField
    , DefineIndexField
    -- * Request Lenses
    , defeDomainName
    , defeIndexField

    -- * Destructuring the Response
    , defineIndexFieldResponse
    , DefineIndexFieldResponse
    -- * Response Lenses
    , defrsStatus
    , defrsIndexField
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.CloudSearch.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the 'DefineIndexField' operation.
-- Specifies the name of the domain you want to update and the index field
-- configuration.
--
-- /See:/ 'defineIndexField' smart constructor.
data DefineIndexField = DefineIndexField'
    { _defeDomainName :: !Text
    , _defeIndexField :: !IndexField
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DefineIndexField' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'defeDomainName'
--
-- * 'defeIndexField'
defineIndexField
    :: Text -- ^ 'defeDomainName'
    -> IndexField -- ^ 'defeIndexField'
    -> DefineIndexField
defineIndexField pDomainName_ pIndexField_ =
    DefineIndexField'
    { _defeDomainName = pDomainName_
    , _defeIndexField = pIndexField_
    }

-- | Undocumented member.
defeDomainName :: Lens' DefineIndexField Text
defeDomainName = lens _defeDomainName (\ s a -> s{_defeDomainName = a});

-- | The index field and field options you want to configure.
defeIndexField :: Lens' DefineIndexField IndexField
defeIndexField = lens _defeIndexField (\ s a -> s{_defeIndexField = a});

instance AWSRequest DefineIndexField where
        type Rs DefineIndexField = DefineIndexFieldResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DefineIndexFieldResult"
              (\ s h x ->
                 DefineIndexFieldResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "IndexField"))

instance ToHeaders DefineIndexField where
        toHeaders = const mempty

instance ToPath DefineIndexField where
        toPath = const "/"

instance ToQuery DefineIndexField where
        toQuery DefineIndexField'{..}
          = mconcat
              ["Action" =: ("DefineIndexField" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _defeDomainName,
               "IndexField" =: _defeIndexField]

-- | The result of a 'DefineIndexField' request. Contains the status of the
-- newly-configured index field.
--
-- /See:/ 'defineIndexFieldResponse' smart constructor.
data DefineIndexFieldResponse = DefineIndexFieldResponse'
    { _defrsStatus     :: !Int
    , _defrsIndexField :: !IndexFieldStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DefineIndexFieldResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'defrsStatus'
--
-- * 'defrsIndexField'
defineIndexFieldResponse
    :: Int -- ^ 'defrsStatus'
    -> IndexFieldStatus -- ^ 'defrsIndexField'
    -> DefineIndexFieldResponse
defineIndexFieldResponse pStatus_ pIndexField_ =
    DefineIndexFieldResponse'
    { _defrsStatus = pStatus_
    , _defrsIndexField = pIndexField_
    }

-- | The response status code.
defrsStatus :: Lens' DefineIndexFieldResponse Int
defrsStatus = lens _defrsStatus (\ s a -> s{_defrsStatus = a});

-- | Undocumented member.
defrsIndexField :: Lens' DefineIndexFieldResponse IndexFieldStatus
defrsIndexField = lens _defrsIndexField (\ s a -> s{_defrsIndexField = a});
